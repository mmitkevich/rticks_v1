#pragma once
#include "events.h"

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
        return data[data.size()-1-i];
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

    MetricsMap(CharacterVector symbols)
        :symbols(symbols) {
    }

    List toR() {
      List lst;
      int total_rows = 0;
      std::vector<double> time_out;
      std::vector<const char*> symbols_out;
      typedef std::map<std::string, std::vector<double> > MetricsOut;
      MetricsOut metrics_out;
      for(auto kv : *this) {
          Metric &m = kv.second;
          for(int i=0;i<m.size();i++) // by stocks
              for(int j=0; j<m[i].size(); j++) // by time
              {
                  metrics_out[kv.first].push_back(m[i][j]);
                  symbols_out.push_back((const char*)symbols[i]);
              }
      }
      for(auto & kv2: metrics_out) {
          lst[kv2.first] = NumericVector(kv2.second.begin(), kv2.second.end());
      }
      return lst;
    }

    Metric make_metric(std::string name, double empty = NAN) {
      Metric metric(symbols.size(), empty);
      insert(value_type(name, metric));
      return metric;
    }

    void flush() {
        for(auto kv: *this) {
          kv.second.flush();
        }
    }
};
  //typedef RowWrapper<DataFrame, Metrics, double, NumericVector> NumericMetric;
template<typename TInput=OrderFilled>
struct Metrics : public Algo,
        public MetricsMap,
        public Observer<TInput>
{
  int index;
  int stop;

  Window<double> time;
  Metric pnl, pnl_h, pnl_l;  // by symbol
  Metric rpnl;
  Metric pos, pos_h, pos_l;
  Metric qty_buy, qty_sell;
  Metric roundtrips;
  Datetime next_flush_dt;

  Metrics(DataFrame params, List config)
    : Algo(params, config),
      MetricsMap(required<CharacterVector>(params, "symbol")),
      index(0), stop(0), next_flush_dt(NAN),

      // initialize metrics
      pnl(make_metric("pnl")),
      pnl_h(make_metric("pnl.high")),
      pnl_l(make_metric("pnl.low")),
      rpnl(make_metric("rpnl")),
      pos(make_metric("pos")),
      pos_h(make_metric("pos.high")),
      pos_l(make_metric("pos.low")),
      qty_buy(make_metric("qty.buy")),
      qty_sell(make_metric("qty.sell")),
      roundtrips(make_metric("roundtrips"))
  {  }

  void on_next(TInput fill) {
    if(next_flush_dt.is_na()){
        next_flush_dt = fill.datetime; // FIXME: convert to flush time
    }
    int i = fill.symbol;
    // update pos
    pos[i] += fill.qty;
    pos_l[i] = std::min<double>(pos_l[i], pos[i]);
    pos_h[i] = std::max<double>(pos_h[i], pos[i]);

    // update qty bought/sold
    (fill.qty > 0 ? qty_buy : qty_sell)[i] += fabs(fill.qty);

    // update free cash
    rpnl[i] -= fill.qty * fill.price;

    if(is_zero(pos[i]))
      roundtrips[i]++;

    flush(fill.datetime);
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
