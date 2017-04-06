#pragma once

#include "algo.h"

namespace Rcpp {

template<typename TExecutionMessage=ExecutionMessage,
         typename TSessionMessage=SessionMessage,
         typename TQuoteMessage=QuoteMessage>
struct Metrics : public Algo,
        public IObserver<TExecutionMessage>,
        public IObserver<TSessionMessage>,
        public IObserver<TQuoteMessage>
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
  BuySellVector market;
  
  CharacterVector symbols;
  std::vector<std::tuple<std::string, double, NumericVector*>> metrics;

  List perfs;
  List trades;

  double next_flush_dt;
  long perfs_interval = 24*60*60;

  Metrics(DataFrame params, List config, std::string name="metrics")
    : Algo(params, config, name),
      symbols(required<CharacterVector>(params, "symbol")),
      index(0), stop(10), next_flush_dt(NAN),
      pnl(params.nrows(), 0.0),
      market(params.nrows()),
      perfs_interval((long)roundl(optional<NumericVector>(config, "perfs_freq", 24*60*60)[0])),
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
      //if(logger) {
      //  logger->warn("perfs_freq={}", perfs_interval);
      //  logger->flush();
      //}
      if(perfs_interval<1){
        throw std::runtime_error("config$perfs freq should be like 1h=3600, 1d=86400");
      }
  }

  template<typename TMarket>
  void on_init(TMarket &market) {
    market.$execs >>= *this;
    market.$quotes >>= *this;
    market.$session >>= *this;
  }

  void init_metric(NumericVector* var, std::string name, double initial=NAN) {
      metrics.push_back(std::make_tuple(name, initial, var)); // save reference
  }

  virtual void on_next(TSessionMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    set_flush_time();
    try_flush();
  }

  void set_flush_time() {
    if(std::isnan(next_flush_dt)) {
      next_flush_dt = datetime(); // FIXME: convert to flush time
      next_flush_dt -= ((long)next_flush_dt) % perfs_interval;
      next_flush_dt = truncl(next_flush_dt);      // flush_dt = 00:00 UTC
//      flush_perfs();  // flush initial zeros
      next_flush_dt = next_flush_dt + perfs_interval;
    }
  }

  void update_hl(int s) {
    assert(!std::isnan(pnl[s]));
    pos_l[s] = std::min<double>(pos_l[s], pos[s]);
    pos_h[s] = std::max<double>(pos_h[s], pos[s]);
    pnl_l[s] = std::min<double>(pnl_l[s], pnl[s]);
    pnl_h[s] = std::max<double>(pnl_h[s], pnl[s]);
  }
  
  virtual void on_next(TQuoteMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    market.update(e.symbol, e);
    auto s = e.symbol;
    pnl[s] = cash[s] + pos[s] * e.price * multiplier[s];
    update_hl(e.symbol);    
    xlog<debug>("PNL.Q", e.symbol, e.price, e.qty);
    try_flush();   
  }
  
  virtual void on_next(TExecutionMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    //dlog<3>(e);
    //if(fabs(e.qty)>3) {
    //    xlog<1>() << "LARGE TRADE "
    //              << e
    //              << std::endl;
    //}
    //set_flush_time();

    auto s = e.symbol;

    // update pos
    pos[s] = pos[s] + e.qty;

    // update qty bought/sold
    (e.qty > 0 ? qty_buy : qty_sell)[s] += fabs(e.qty);

    // update free cash
    cash[s] = cash[s] - e.qty * e.fill_price * multiplier[s];
    assert(!std::isnan(cash[s]));

    pnl[s] = cash[s] + pos[s] * e.fill_price * multiplier[s];

    update_hl(s);
    //if(is_zero(pos[s]))
    //  roundtrips[s] = roundtrips[s] + 1;

    xlog<info>("PNL.E", s, e.price, e.qty);
      
    try_flush();
  }
  
  template<int level>
  void xlog(const char*what, SymbolId s, double fill_price=NAN, double fill_qty=NAN) {
    if(level>=log_level) {
      if(logger) {
        auto time = std::isnan(dt) ? std::string("NA") : Datetime(dt).format();
        logger->log(spdlog::level::info, "{} | {} | {}={} | M={}, {} | PNL={} | PNL.LH={} {} | CASH={} | POS={} | POS.LH={} {} | QTY={}, {} | {} | {}", // FIXME ::(spdlog::level::level_enum)level
                    time, what, s.id, s.index, market.buy[s], market.sell[s], pnl[s], pnl_l[s], pnl_h[s], cash[s], pos[s], pos_l[s], pos_h[s], qty_buy[s], qty_sell[s], fill_price, fill_qty);
        //if(level>=log_flush_level)
        //  logger->flush();
      }
    }
  }
  
  void try_flush() {
    if(!std::isnan(next_flush_dt)){
        while(dt >= next_flush_dt-eps()) {
          flush_perfs();
          next_flush_dt = next_flush_dt + perfs_interval;
        }
    }
  }

  void flush_perfs() {
   
    logger->debug("rticks::Metrics::flush_perfs {}", Datetime(dt));

    for(int s=0;s<symbols.size();s++) {
      update_hl(s);
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
