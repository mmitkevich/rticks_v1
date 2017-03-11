#pragma once

#include "algo.h"

namespace Rcpp {

const int SECONDS_PER_DAY = 24*60*60;

template<typename TExecutionMessage=ExecutionMessage,
         typename TSessionMessage=SessionMessage>
struct Metrics : public Algo,
        public IObserver<TExecutionMessage>,
        public IObserver<TSessionMessage>
{
  size_t index;
  size_t stop;

  NumericVector date;
  NumericVector pnl, pnl_h, pnl_l;  // by symbol
  NumericVector cash;
  NumericVector pos, pos_h, pos_l;
  NumericVector qty_buy, qty_sell;
  NumericVector multiplier;
  //NumericVector roundtrips;

  CharacterVector symbols;
  std::vector<std::tuple<std::string, double, NumericVector*>> metrics;

  List perfs;
  List trades;

  double next_flush_dt;


  Metrics(DataFrame params, List config, std::string name="metrics")
    : Algo(params, config, name),
      symbols(required<CharacterVector>(params, "symbol")),
      index(0), stop(10), next_flush_dt(NAN),
      pnl(params.nrows(), 0.0),
      multiplier(required<NumericVector>(params, "multiplier")),
      pos(optional<NumericVector>(params, "pos", 0.0)),
      cash(optional<NumericVector>(params, "cash", 0.0)), // cash that was paid for the pos (or average price of pos)
      pos_h(params.nrows()),
      pos_l(params.nrows()),
      pnl_h(params.nrows()),
      pnl_l(params.nrows()),
      qty_buy(optional<NumericVector>(params, "qty_buy", 0.0)),
      qty_sell(optional<NumericVector>(params, "qty_sell", 0.0))
  {
      // initialize metrics
      init_metric(&pnl, "pnl");
      init_metric(&cash, "cash");
      init_metric(&pos, "pos");
      init_metric(&pnl_h, "pnl_high", -INFINITY);
      init_metric(&pnl_l, "pnl_low",  +INFINITY);
      init_metric(&pos_h, "pos_high", -INFINITY);
      init_metric(&pos_l, "pos_low",  +INFINITY);
      init_metric(&qty_buy, "qty_buy");
      init_metric(&qty_sell, "qty_sell");
      //init_metric(&roundtrips, "roundtrips", 0.0);
      perfs_nrows(stop);  // datetime, symbol, value
  }

  template<typename TMarket>
  void on_init(TMarket &market) {
    market.$execs >>= *this;
    market.$session >>= *this;
  }

  void init_metric(NumericVector* var, std::string name, double initial=NAN) {
      metrics.push_back(std::make_tuple(name, initial, var)); // save reference
  }

  virtual void on_next(TSessionMessage e) {
    on_clock(e.rtime);
    //dlog<1>(e);
    set_flush_time();
  }

  void set_flush_time() {
    if(std::isnan(next_flush_dt)) {
      next_flush_dt = datetime(); // FIXME: convert to flush time
      next_flush_dt -= ((long)next_flush_dt) % SECONDS_PER_DAY;
      next_flush_dt = truncl(next_flush_dt);      // flush_dt = 00:00 UTC
//      flush_perfs();  // flush initial zeros
      next_flush_dt = next_flush_dt + SECONDS_PER_DAY;
    }
  }

  virtual void on_next(TExecutionMessage e) {
    on_clock(e.rtime);
    //dlog<3>(e);
    //if(fabs(e.qty)>3) {
    //    xlog<1>() << "LARGE TRADE "
    //              << e
    //              << std::endl;
    //}
    //set_flush_time();

    int s = e.symbol;

    // update pos
    pos[s] = pos[s] + e.qty;
    pos_l[s] = std::min<double>(pos_l[s], pos[s]);
    pos_h[s] = std::max<double>(pos_h[s], pos[s]);

    // update qty bought/sold
    (e.qty > 0 ? qty_buy : qty_sell)[s] += fabs(e.qty);

    // update free cash
    cash[s] = cash[s] - e.qty * e.price * multiplier[s];
    assert(!std::isnan(cash[s]));

    pnl[s] = cash[s] + pos[s] * e.price * multiplier[s];
    assert(!std::isnan(pnl[s]));
    pnl_l[s] = std::min<double>(pnl_l[s], pos[s]);
    pnl_h[s] = std::max<double>(pnl_h[s], pos[s]);

    //if(is_zero(pos[s]))
    //  roundtrips[s] = roundtrips[s] + 1;

    try_flush();
  }

  void try_flush() {
    if(!std::isnan(next_flush_dt)){
        while(dt >= next_flush_dt-eps()) {
          flush_perfs();
          next_flush_dt = next_flush_dt + SECONDS_PER_DAY;
        }
    }
  }

  void flush_perfs() {
   
    logger->debug("rticks::Metrics::flush_perfs {}", Datetime(dt));

    for(int s=0;s<symbols.size();s++) {
        pos_l[s] = std::min<double>(pos_l[s], pos[s]);
        pos_h[s] = std::max<double>(pos_h[s], pos[s]);
        pnl_l[s] = std::min<double>(pnl_l[s], pnl[s]);
        pnl_h[s] = std::max<double>(pnl_h[s], pnl[s]);
    }
    
    std::string name;
    double initial;
    NumericVector *metric;
    for(auto tup : metrics) {
        std::tie(name, initial, metric) = tup;
        for(int i=0; i < symbols.size(); i++) {
          as<NumericVector>(perfs[0])[index] = next_flush_dt;
          as<CharacterVector>(perfs[1])[index] = (const char*)symbols[i];
          as<CharacterVector>(perfs[2])[index] = (const char*)name.c_str();
          as<NumericVector>(perfs[3])[index] = (*metric)[i];

          index++;
          if(index>=stop) {
            stop = 2*stop;
            perfs_nrows(stop);
          }

        }
        auto is_cumulative = std::isnan(initial);
        if(!is_cumulative)
          *metric = initial;
    }
  }

  List toR() {
    List result;
    perfs_nrows(index);
    //if(index>0)
    perfs.attr("names") = CharacterVector::create("datetime", "symbol", "metric", "value");
    //perfs.attr("class") = "data.frame";
    result.push_back(pos,"pos");
    result.push_back(pnl, "pnl");
    result.push_back(cash,"cash");
    result.push_back(perfs, "perfs");
    result.push_back(qty_buy, "qty_buy");
    result.push_back(qty_sell, "qty_sell");
    return result;
  }

  void perfs_nrows(size_t size) {
    set_nrows<NumericVector, CharacterVector, CharacterVector, NumericVector>(perfs, size);
  }
};
} //namespace Rcpp
