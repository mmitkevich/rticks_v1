#pragma once

#ifndef ALGO_H
#define ALGO_H

#include "reactive.h"
#include <cmath>

namespace Rcpp {



namespace OrderSide {
  enum {
    BUY = 1,
    SELL = -1,
    UNKNOWN = 0
  };
};

struct SymbolId {
  const char* id;
  int index;

  SymbolId(const char* id=NULL, int index=-1)
    : id(id), index(index) { }

  SymbolId(const SymbolId &rhs)
      :id(rhs.id), index(rhs.index) {

  }

  operator int() const {
    return index;
  }
};
struct Message;

struct IAlgo {
    enum {
        EMPTY  =  0x0001
    };

    virtual void on_execute() = 0;  // eat from input queues, notify subscribers.
    virtual double datetime() const = 0;   // current timestamp
    virtual ~IAlgo(){ }
    virtual bool operator<(IAlgo &rhs) const{
        return datetime() < rhs.datetime();
    }
};

struct Algo : public IAlgo {
  List config;
  DataFrame params;
  double dt;
  std::string name;
  int log_level;
  bool log_flush_level;
  
  Algo(
    DataFrame params, // (symbol, mpi, par1, par2,....)
    List config,
    std::string name = "")
      : config(config),
        params(params),
        dt(NAN),
        name(name)
  {
    log_level = optional<IntegerVector>(config, "log_level", spdlog::level::info)[0];
    log_flush_level  = optional<IntegerVector>(config, "log_flush_level", spdlog::level::warn)[0];
    std::cout << name <<" log_level=" << log_level << std::endl;
  }

  int size() {
      return params.size();
  }

  virtual double datetime() const {
     return dt;
  }

  virtual void on_execute() {

  }

  template<typename TMessage>
  void verify(const TMessage &e) {
      if(std::isnan(e.rtime) || std::isnan(e.ctime)) {
          std::stringstream ss;
          ss << "NAN time ctime="<<e.ctime<<", rtime="<<e.rtime;
          ss <<", msg="<<e<<"\n";
          auto s = ss.str();
          std::cerr << s;
          throw std::runtime_error(s);
      }
  }

  virtual void on_clock(double dtime) {
      assert(!std::isnan(dtime));
      if(std::isnan(dtime)) {
          throw new std::runtime_error("on_clock(NAN)");
      }else if(!std::isnan(dt) && dtime<dt-eps()) {
          throw new std::runtime_error("on_clock("+name+", "+Datetime(dtime).format()+" is less than current "+Datetime(dt).format());
      }
      dt = dtime;
  }

  enum {
    trace = spdlog::level::trace,
    debug = spdlog::level::debug,
    info = spdlog::level::info,
    warn = spdlog::level::warn,
    error = spdlog::level::critical
  };
  
  template<int level, typename TMessage>
  void dlog(const TMessage &e) {
      assert(!std::isnan(datetime()));
      if(level>=log_level) {
        if(logger) {
          auto my_time = std::isnan(datetime()) ? std::string("NA") : Datetime(datetime()).format();
          auto time = std::isnan(e.rtime) ? std::string("NA"): Datetime(e.rtime).format();
          logger->log(spdlog::level::info, "{} | {} | {} | {}\n", my_time, name, time, e);//(spdlog::level::level_enum)level //FIXME
          if(level>=log_flush_level)
            logger->flush();
        }
        //else
        //std::cout << Datetime(e.rtime) << "|" << name << " | " << e <<std::endl<<std::flush;
      }
      verify(e);
  }
};

template<typename TAlgo=Algo>
struct Scheduler: public Algo, public IScheduler<TAlgo> {
    struct algo_less {
        bool operator()( const TAlgo* lhs, const TAlgo* rhs ) const {
            return lhs->datetime() < rhs->datetime();
        }
    };

    typedef std::queue<TAlgo*> queue_type;
    std::deque<TAlgo*> queue;

    Scheduler(DataFrame params, List config, std::string name = "scheduler")
        :Algo(params, config, name) { }

    virtual void on_schedule(TAlgo* algo, double dt) {
        // TODO: if prev!=NAN it could be used to faster find algo in the queue
        auto it = std::find(queue.begin(), queue.end(), algo);
        if(it!=queue.end()) {
            queue.erase(it);    // remove first
        }
        /*if(log_level<=info) {
            std::cout << "on_schedule "<< algo->name;
            if(std::isnan(dt)){
                std::cout << " NAN";
            }else {
                std::cout << Datetime(dt);
            }
            std::cout << std::endl << std::flush;
        }*/

        if(!std::isnan(dt)) { // reschedule if needed

            algo->on_clock(dt); // update tartet time
            auto jt = std::upper_bound(queue.begin(), queue.end(), algo, algo_less()); // who after us
            queue.insert(jt, algo); // stand before him
        }
    }

    void on_execute(double time) {
        while(!queue.empty()) {
            auto algo = queue.front();
            auto t = algo->datetime();
            assert(!std::isnan(t));
            if(algo->datetime() <= time) {
                algo->on_execute();
            }else
                break;
        }
    }
};

template<typename TMessage=Message,
         typename TScheduler=Scheduler<Algo>,
         typename TObserver=IObserver<TMessage>,
         typename TBase=Stream<TMessage,TObserver> >
struct LatencyQueue : public Algo,
              public TBase
{
    using TBase::notify;

    std::queue<TMessage> queue;
    TScheduler *scheduler;
    NumericVector latency;

    LatencyQueue(DataFrame params, List config, TScheduler *scheduler, std::string name)
        : Algo(params, config, name),
          latency(optional<NumericVector>(params, std::string("latency.")+name, NumericVector(params.nrows(), 1e-6))),
          scheduler(scheduler) {
    }

    /// will push to queue and will not process until scheduled
    virtual void on_next(TMessage e) {
        auto g = std::move(e);
        verify(g);
        g.rtime += latency[g.symbol];
        queue.push(std::move(g));
        on_clock(g.symbol);
    }

    void on_clock(int s) {
        auto ndt = queue.empty() ? NAN : queue.front().rtime;
        scheduler->on_schedule(this, ndt);
    }

    // we got scheduled
    virtual void on_execute() {
        if(!queue.empty()) {
            auto e = std::move(queue.front());
            queue.pop();
            //dlog<info>(e);
            on_clock(e.symbol);
            notify(std::move(e));
        }
    }

    //virtual void notify(TObserver *obs, TMessage e) {
    //    Algo *algo = static_cast<Algo*>(obs);
    //    algo->on_clock(e.datetime);
    //}

    virtual bool empty() {
        return queue.empty();
    }
};

/*std::ostream & operator<< (std::ostream &os, const SymbolId &s) {
    if(s.id!=NULL)
        os << s.id;
    else
        os << "?";
    os << "#";
    if(s.index!=-1)
        os << s.index;
    else
        os << "?";
    return os;
}*/


struct BuySell {
  double buy;
  double sell;

  BuySell()
    : buy(NAN), sell(NAN) { }

  BuySell(double buy, double sell)
    : buy(buy), sell(sell) { }

  BuySell(const BuySell &rhs)
    : buy(rhs.buy), sell(rhs.sell) { }

  BuySell &operator=(const BuySell &rhs) {
    buy = rhs.buy;
    sell = rhs.sell;
    return *this;
  }

  int count_buy() const {
    return std::isnan(buy) ? 0:1;
  }

  int count_sell() const {
    return std::isnan(sell) ? 0:1;
  }

  int count(int side) const {
      return side>0 ? count_buy() : count_sell();
  }

  double slippage(int side, double qty) const {
    return NAN;
  }

  double average(int side, double qty) const {
    return (*this)(side);
  }

  double& operator()(int side) {
    return side>0 ? buy:sell;
  }

  double operator()(int side) const {
    return side>0 ? buy:sell;
  }

};

template<typename TValue,
         typename TVector>
struct vector_iterator {
    TVector *parent;
    int index;
    typedef TValue value_type;

    vector_iterator(TVector *p, int index)
        : parent(p), index(index) { }

    vector_iterator(const vector_iterator &rhs)
      : parent(rhs.parent),
        index(rhs.index)
    { }

    bool operator==(const vector_iterator &rhs){
        return index==rhs.index;
    }

    TValue operator*() {
        return parent[index];
    }
};

template<typename TValue, typename TVector>
struct vector_base
{
    typedef vector_iterator<TValue, TVector> iterator;

    iterator begin() {
      return iterator(self());
    }

    iterator end() {
        return iterator(self(), self()->size());
    }
private:
    TVector* self() {
        return static_cast<TVector*>(this);
    }
};

struct BuySellVector : public vector_base<BuySell, BuySellVector> {
  NumericVector buy;
  NumericVector sell;

  int size() const {
      return buy.size();
  }

  BuySellVector(NumericVector buy, NumericVector sell)
    : buy(std::move(buy)),
      sell(std::move(sell)) {
  }

  BuySellVector(int n)
    : buy(n,NAN),
      sell(n,NAN) {
  }

  BuySellVector() {
    buy = NAN;
    sell = NAN;
  }

  BuySellVector(const BuySellVector& rhs) = default;


  const BuySell operator[](int index) const {
    return BuySell(buy[index], sell[index]);
  }

  double midprice(SymbolId s) {
    auto m = (*this)[s];
    if(m.count_buy() && m.count_sell())
      return 0.5 * (m.buy + m.sell);
    else
      assert(false);
    return NAN;
  }
  
  
  template<typename TMessage>
  void update(int index, const TMessage &e) {
    if(e.side()>0)
        buy[index] = e.price;
    else
        sell[index] = e.price;
  }

  void reset(int index, int side, double price) {
      if(side>0)
          buy[index] = price;
      else if(side<0)
          sell[index] = price;
  }

  NumericVector& operator()(int side) {
    if(side>0)
      return buy;
    return sell;
  }
};

}; //ns Rcpp

#endif // ALGO_H

