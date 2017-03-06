#pragma once

namespace Rcpp {

const int SECONDS_PER_DAY = 24*60*60;
#if 0
template<typename TValue>
struct Window  {
    std::deque<TValue> data;
    TValue empty;
    int length;

    Window(TValue empty = 0, int length = 0)
        : empty(empty),
          length(length)
    {
        data.push_back(empty);
    }

    Window(const Window &rhs) = default;

    operator TValue&() {
        return data[data.size()-1];
    }

    Window &operator=(TValue val) {
        data[data.size()-1] = val;
        return *this;
    }

    void flush() {
        data.push_back(data[data.size()-1]);
        if(length>1 && data.size()>length){
            data.pop_front();
        }
    }

    void zero() {
        data[data.size()-1] = empty;
    }

    TValue operator[](int i) const {
        if(i>=data.size()-1 || i<0) {
            return empty;
        }
        return data[i];
    }

    int size() const {
        return data.size();
    }

    NumericVector toR() {
        return NumericVector(data.begin(), data.end());
    }
};

template<typename TValue>
struct WindowList {
    std::vector<Window<TValue>> data;

    WindowList(int n, TValue empty = 0, int length=0) {
    //void resize(int n, TValue empty = 0, int length=0) {
        for(int i=0; i<n; i++)
            data.push_back(Window<TValue>(empty, length));
    }

    WindowList(const WindowList &rhs) = default;

    Window<TValue>& operator[](int i) {
        return data[i];
    }

    int size() const {
        return data.size();
    }

    void flush() {
        for(auto& v: data) {
            v.flush();
        }
    }
};

typedef WindowList<double> Metric;    // for each security
struct MetricsMap : public std::map<std::string, Metric> {
    CharacterVector symbols;
    Window<double> time;

    MetricsMap(CharacterVector symbols)
        :symbols(symbols) {
    }
    MetricsMap(const MetricsMap& rhs) = default;

    List toR() {
      List lst;
      int total_rows = 0;
      std::vector<double> time_out;
      std::vector<const char*> symbols_out;
      typedef std::map<std::string, std::vector<double> > MetricsOut;
      MetricsOut metrics_out;
      for(auto &kv: *this) {
          Metric &m = kv.second;
          for(int i=0;i<m.size();i++) // by stocks
              for(int j=0; j<m[i].size(); j++) // by time
              {
                  metrics_out[kv.first].push_back(m[i].data[j]);
                  symbols_out.push_back((const char*)symbols[i]);
                  time_out.push_back(time.data[j]);
              }
      }
      for(auto & kv2: metrics_out) {
          lst[kv2.first] = NumericVector(kv2.second.begin(), kv2.second.end());
      }
      lst["symbol"] = CharacterVector(symbols_out.begin(), symbols_out.end());
      lst["datetime"] = NumericVector(time_out.begin(), time_out.end());
      return lst;
    }

    Metric make_metric(std::string name, double empty = NAN) {
      Metric metric(symbols.size(), empty);
      insert(value_type(name, metric));
      return metric;
    }

    void flush() {
        time.flush();
        for(auto &kv: *this) {
          kv.second.flush();
        }
    }
};
#endif

template<typename TExecutionMessage=ExecutionMessage>
struct Metrics : public Algo,
        public IObserver<TExecutionMessage>
{
  typedef DFRow<double, NumericVector, List> Metric;

  size_t index;
  size_t stop;

  NumericVector date;
  Metric pnl, pnl_h, pnl_l;  // by symbol
  Metric rpnl;
  Metric pos, pos_h, pos_l;
  Metric qty_buy, qty_sell;
  Metric roundtrips;

  CharacterVector symbols;
  std::vector<Metric*> metrics;

  double next_flush_dt;


  Metrics(DataFrame params, List config, std::string name="metrics")
    : Algo(params, config, name),
      symbols(required<CharacterVector>(params, "symbol")),
      index(0), stop(10), next_flush_dt(NAN)
  {
      date = NumericVector(stop);
      // initialize metrics
      init_metric(&pnl,     "pnl").set(optional<NumericVector>(params, "pnl", 0.0));
      init_metric(&pnl_h,   "pnl.high", -INFINITY);
      init_metric(&pnl_l,   "pnl.low",  +INFINITY);
      init_metric(&rpnl,    "rpnl", 0);
      init_metric(&pos,     "pos").set(optional<NumericVector>(params, "pos", 0.0));
      init_metric(&pos_h,   "pos.high", -INFINITY);
      init_metric(&pos_l,   "pos.low",  +INFINITY);
      init_metric(&qty_buy,     "qty.buy", 0);
      init_metric(&qty_sell,    "qty.sell", 0);
      init_metric(&roundtrips,  "roundtrips", 0);
  }

  Metric &init_metric(Metric* var, std::string name, double na=NAN) {
      *var = std::move(Metric(symbols, stop, na, &index)); // move metric into var
      var->name = name;
      metrics.push_back(var); // save reference
      return *var;
  }

  void on_next(TExecutionMessage e) {
    on_clock(e.rtime);
    dlog<3>(e);
    if(fabs(e.qty)>3) {
        std::cout << "BIG DEAL "
                  <<e
                 <<std::endl;
    }
    if(std::isnan(next_flush_dt)) {
        next_flush_dt = e.rtime; // FIXME: convert to flush time
        next_flush_dt -= ((long)next_flush_dt) % SECONDS_PER_DAY;
        next_flush_dt = truncl(next_flush_dt);      // flush_dt = 00:00 UTC
    }

    int s = e.symbol;

    date[index] = e.rtime;
    // update pos
    pos[s] = pos[s] + e.qty;
    pos_l[s] = std::min<double>(pos_l[s], pos[s]);
    pos_h[s] = std::max<double>(pos_h[s], pos[s]);

    // update qty bought/sold
    (e.qty > 0 ? qty_buy : qty_sell)[s] += fabs(e.qty);

    // update free cash
    rpnl[s] = rpnl[s] - e.qty * e.price;
    assert(!std::isnan(rpnl[s]));

    pnl[s] = rpnl[s] + pos[s] * e.price;
    assert(!std::isnan(pnl[s]));
    pnl_l[s] = std::min<double>(pnl_l[s], pos[s]);
    pnl_h[s] = std::max<double>(pnl_h[s], pos[s]);

    if(is_zero(pos[s]))
      roundtrips[s] = roundtrips[s] + 1;

    if(dt >= next_flush_dt) {
        flush_metrics();
        next_flush_dt = next_flush_dt + SECONDS_PER_DAY;
    }
  }

  void nrows_metrics(size_t size) {
      stop = size;

      NumericVector newdate(size);
      for(int i=0; i<size; i++)
          newdate[i] = date[i];

      date = newdate;

      for(Metric *metric : metrics) {
          metric->nrows(size);
      }
  }

  void flush_metrics() {
    index++;
    if(index>=stop) {
        nrows_metrics(2*stop);
    }
    for(Metric * metric: metrics) {
        auto is_cumulative = std::isnan(metric->na);
        for(int i=0; i<metric->size(); i++)
            metric->set(i, is_cumulative ? metric->get(i, 1) : metric->na);
    }
  }

  List toR() {
    List result;
    nrows_metrics(index+1);
    for(int s=0; s<metrics.size(); s++) {
        Metric *metric = metrics[s];
        List df = metric->data;
        df.attr("names") = symbols;
        //df.attr("class") = "data.frame";
        result[metric->name] = df;
    }
    result["datetime"] = date;
    return result;
  }
};

#if 0
template<typename TExecutionMessage=ExecutionMessage>
struct NewMetrics : public Algo, public Stream<TExecutionMessage>
{
    DynamicObservable<double> pos;
    //Map<std::function<double(TExecutionMessage)>, TExecutionMessage, double > pos;

    NewMetrics(DataFrame params, List config, std::string name)
     : Algo(params, config, name),
       pos(std::move(as_dynamic(*this % map([](TExecutionMessage e){return e.qty;}))))
    {
    }
};
#endif
} //namespace Rcpp
