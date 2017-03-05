#pragma once

namespace Rcpp {

const int SECONDS_PER_DAY = 24*60*60;

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
  //typedef RowWrapper<DataFrame, Metrics, double, NumericVector> NumericMetric;
template<typename TExecutionMessage=ExecutionMessage>
struct Metrics : public Algo,
        public MetricsMap,
        public IObserver<TExecutionMessage>
{
  int index;
  int stop;

  Metric pnl, pnl_h, pnl_l;  // by symbol
  Metric rpnl;
  Metric pos, pos_h, pos_l;
  Metric qty_buy, qty_sell;
  Metric roundtrips;
  Datetime next_flush_dt;

  Metrics(DataFrame params, List config, std::string name="metrics")
    : Algo(params, config, name),
      MetricsMap(required<CharacterVector>(params, "symbol")),
      index(0), stop(0), next_flush_dt(NAN),

      // initialize metrics
      pnl(make_metric("pnl",0)),
      pnl_h(make_metric("pnl.high",-INFINITY)),
      pnl_l(make_metric("pnl.low",+INFINITY)),
      rpnl(make_metric("rpnl",0)),
      pos(make_metric("pos", 0)),
      pos_h(make_metric("pos.high",-INFINITY)),
      pos_l(make_metric("pos.low",+INFINITY)),
      qty_buy(make_metric("qty.buy",0)),
      qty_sell(make_metric("qty.sell",0)),
      roundtrips(make_metric("roundtrips",0))
  {  }

  void on_next(TExecutionMessage e) {
    on_clock(e.rtime);
    dlog<3>(e);
    if(fabs(e.qty)>3) {
        std::cout << "BIG DEAL "
                  <<e
                 <<std::endl;
    }
    if(next_flush_dt.is_na()){
        next_flush_dt = e.rtime; // FIXME: convert to flush time
    }
    int s = e.symbol;
    // update pos
    pos[s] += e.qty;
    pos_l[s] = std::min<double>(pos_l[s], pos[s]);
    pos_h[s] = std::max<double>(pos_h[s], pos[s]);
    time = e.rtime;

    // update qty bought/sold
    (e.qty > 0 ? qty_buy : qty_sell)[s] += fabs(e.qty);

    // update free cash
    rpnl[s] -= e.qty * e.price;

    if(is_zero(pos[s]))
      roundtrips[s]++;

    flush(e.rtime);
    //    std::cout << t << " | " << q << " * " << price << " | " << pos << " | " << cash << "\n";
  }

  void flush(Datetime datetime) {
      if(datetime >= next_flush_dt) {
          MetricsMap::flush();
          next_flush_dt = next_flush_dt + SECONDS_PER_DAY;
      }
  }

};

} //namespace Rcpp
