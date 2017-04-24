#pragma once

#include "algo.h"

namespace Rcpp {

const long METRICS_DEFAULT_TIMEFRAME = 24*60*60;
  
template<typename TExecutionMessage=ExecutionMessage,
         typename TSessionMessage=SessionMessage,
         typename TQuoteMessage=QuoteMessage,
         typename TOrderMessage=OrderMessage>
struct Metrics : public MarketAlgo,
        public IObserver<TExecutionMessage>,
        public IObserver<TSessionMessage>,
        public IObserver<TQuoteMessage>,
        public IObserver<TOrderMessage>
{
  size_t index;
  size_t stop;

  NumericVector date;
  NumericVector pnl, pnl_h, pnl_l;  // by symbol
  NumericVector cash;
  NumericVector pos_h, pos_l;
  NumericVector qty_buy, qty_sell;
  NumericVector multiplier;
  NumericVector price, price_l, price_h;
  NumericVector buy_l,buy_h;
  NumericVector sell_l,sell_h;
  NumericVector turnover_buy; // dollars bought
  NumericVector turnover_sell; // dollars sold
  
  std::vector<std::tuple<std::string, double, NumericVector*>> metrics;

  List perfs;
  List trades;
  int tz_offset;
  
  double next_flush_dt;
  long perfs_interval = 24*60*60;

  Metrics(DataFrame params, List config, std::string name="metrics")
    : MarketAlgo(params, config, name),
      index(0), stop(10), next_flush_dt(NAN),
      pnl(params.nrows(), 0.0),
      perfs_interval((long)roundl(optional<NumericVector>(config, "perfs_freq", METRICS_DEFAULT_TIMEFRAME)[0])),
      tz_offset((long)optional<IntegerVector>(config, "perfs_tz", 16)[0]),
      multiplier(required<NumericVector>(params, "multiplier")),
      cash(optional<NumericVector>(params, "cash", 0.0)), // cash that was paid for the pos (or average price of pos)
      pos_h(params.nrows()),
      pos_l(params.nrows()),
      pnl_h(params.nrows()),
      pnl_l(params.nrows()),
      price_l(params.nrows()),
      price_h(params.nrows()),
      price(params.nrows()),
      buy_l(params.nrows()),
      buy_h(params.nrows()),
      sell_l(params.nrows()),
      sell_h(params.nrows()),
      turnover_buy(optional<NumericVector>(params, "turnover_buy", 0.0)),
      turnover_sell(optional<NumericVector>(params, "turnover_sell", 0.0)),
      qty_buy(optional<NumericVector>(params, "qty_buy", 0.0)),
      qty_sell(optional<NumericVector>(params, "qty_sell", 0.0))
  {
      // initialize metrics
      init_metric(&cash, "cash");
      
      init_metric(&pnl, "pnl");
      init_metric(&pnl_h, "pnl_high", true, -INFINITY);
      init_metric(&pnl_l, "pnl_low", true,  +INFINITY);
      
      init_metric(&pos, "pos");
      init_metric(&pos_h, "pos_high", true, -INFINITY);
      init_metric(&pos_l, "pos_low",  true, +INFINITY);
      
      init_metric(&price, "price", true, NAN);
      init_metric(&price_h, "price_high", true, -INFINITY);
      init_metric(&price_l, "price_low",  true, +INFINITY);
      
      init_metric(&qty_buy, "qty_buy");
      init_metric(&qty_sell, "qty_sell");

      init_metric(&turnover_buy, "turnover_buy");
      init_metric(&turnover_sell, "turnover_sell");

      init_metric(&buy_h, "buy_high", true, -INFINITY);
      init_metric(&buy_l, "buy_low",  true, +INFINITY);

      init_metric(&sell_h, "sell_high", true, -INFINITY);
      init_metric(&sell_l, "sell_low",  true, +INFINITY);
      
      init_metric(&quotes.buy, "bid");
      init_metric(&quotes.sell, "ask");
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
  void on_init(TMarket &mkt) {
    mkt.$execs >>= *this;
    mkt.$quotes >>= *this;
    mkt.$orders >>= *this;
    mkt.$session >>= *this;
  }

  void init_metric(NumericVector* var, std::string name, bool reset=false, double initial=NAN) {
      metrics.push_back(std::make_tuple(name, initial, var)); // save reference
      if(reset)
          for(int i=0;i<var->size();i++)
              (*var)[i] = initial;
  }
  
  virtual void on_next(TSessionMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    set_flush_time();
    if(!e.is_active())
        flush_perfs();
    else
        try_flush();
  }

  void set_flush_time() {
    if(std::isnan(next_flush_dt)) {
      ; // FIXME: convert to flush time // should be at 16h00 UTC in LONDON
      next_flush_dt = datetime();
      next_flush_dt -= ((long)next_flush_dt) % perfs_interval;
      next_flush_dt = truncl(next_flush_dt);      // flush_dt = 00:00 UTC
//      flush_perfs();  // flush initial zeros
      if(perfs_interval>=86400-eps()) { // for daily we set day cutover time at 
        next_flush_dt += tz_offset*3600;  // london 1600 == 00:00 of this day
      }
      //if(next_flush_dt<datetime()+eps())
      next_flush_dt += perfs_interval;  // only future
      if(logger)
        logger->warn("next_flush_dt {}",Datetime(next_flush_dt));
    }
  }

  void update_hl(int s) {
    assert(!std::isnan(pnl[s]));
    pos_l[s] = std::min<double>(pos_l[s], pos[s]);
    pos_h[s] = std::max<double>(pos_h[s], pos[s]);
    pnl_l[s] = std::min<double>(pnl_l[s], pnl[s]);
    pnl_h[s] = std::max<double>(pnl_h[s], pnl[s]);
    price_l[s] = std::min<double>(price_l[s], market.sell[s]);
    price_h[s] = std::max<double>(price_h[s], market.buy[s]);
  }
  
  virtual void on_next(TQuoteMessage e) {
    on_clock(e.rtime);
    try_flush();   
    
    dlog<debug>(e);
    market.update(e.symbol, e);
    auto s = e.symbol;
    auto px = pos[s]>0 ? market.buy[s] : market.sell[s];
    pnl[s] = cash[s] + pos[s] * px * multiplier[s];
    price[s] = market.midprice(s);
    update_hl(s);    
    xlog<debug>("PNL.Q", e.symbol, e.price, e.qty);
  }
  
  virtual void on_next(TOrderMessage e) {
    on_clock(e.rtime);
    try_flush();
    dlog<debug>(e);
    quotes.update(e.symbol, e);
    xlog<info>("PNL.O", e.symbol, e.price, e.qty);
  }
  
  virtual void on_next(TExecutionMessage e) {
    on_clock(e.rtime);
    try_flush();
    
    dlog<debug>(e);
    //dlog<3>(e);
    //if(fabs(e.qty)>3) {
    //    xlog<1>() << "LARGE TRADE "
    //              << e
    //              << std::endl;
    //}
    //set_flush_time();

    auto s = e.symbol;

    update_hl(s); // for previous position
    // update pos
    pos[s] = pos[s] + e.qty;

    // update qty bought/sold
    (e.qty > 0 ? qty_buy : qty_sell)[s] += fabs(e.qty);

    // update free cash
    cash[s] = cash[s] - e.qty * e.fill_price * multiplier[s];
    assert(!std::isnan(cash[s]));

    auto px = pos[s]>0 ? market.buy[s] : market.sell[s];
    pnl[s] = cash[s] + pos[s] * px * multiplier[s];
    
    if(e.side()>0) {
      buy_h[s] = std::max<double>(buy_h[s], e.price);
      buy_l[s] = std::min<double>(buy_l[s], e.price);
      turnover_buy[s] += e.fill_price*fabs(e.qty);
    }else{
      sell_h[s] = std::max<double>(sell_h[s], e.price);
      sell_l[s] = std::min<double>(sell_l[s], e.price);
      turnover_sell[s] += e.fill_price*fabs(e.qty);
    }
    
    update_hl(s);
    //if(is_zero(pos[s]))
    //  roundtrips[s] = roundtrips[s] + 1;

    xlog<info>("PNL.E", s, e.price, e.qty);
  }
  
  template<int level>
  void xlog(const char*what, SymbolId s, double fill_price=NAN, double fill_qty=NAN) {
    if(level>=log_level) {
      if(logger) {
        auto time = std::isnan(dt) ? std::string("NA") : Datetime(dt).format();
        auto ntime = std::isnan(next_flush_dt) ? std::string("NA") : Datetime(next_flush_dt).format();
        logger->log(spdlog::level::info, "{} | {} | {} | {}={} | M={}, {} | Q={}, {} | S={}, {} | POS={}, {}:{} | PNL={}, {}:{} | CASH={} | QTY={}, {} |  {} | {}", // FIXME ::(spdlog::level::level_enum)level
                    time, what, ntime, s.id, s.index, 
                    market.buy[s], market.sell[s], 
                    quotes.buy[s], quotes.sell[s],
                    stop_quotes.buy[s], stop_quotes.sell[s],
                    pos[s], pos_l[s], pos_h[s],
                    pnl[s], pnl_l[s], pnl_h[s], 
                    cash[s], 
                    qty_buy[s], qty_sell[s], 
                    fill_price, fill_qty);
      }
    }
  }
  
  void try_flush() {
    if(!std::isnan(next_flush_dt)) {
        while(dt >= next_flush_dt-eps()) {
          flush_perfs();
        }
    }
  }

  void flush_perfs() {
    for(int s=0;s<symbols.size();s++) {
      update_hl(s);
      xlog<info>("PNL.FLUSH",SymbolId(symbols[s],s));
    }
    std::string name;
    double initial;
    NumericVector *metric;
    for(auto tup : metrics) {
        std::tie(name, initial, metric) = tup;
        for(int i=0; i < symbols.size(); i++) {
          as<NumericVector>(perfs[0])[index] = next_flush_dt;  // flush 1ns before interval end
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
          for(int i=0; i<symbols.size();i++)
              (*metric)[i] = initial;
    }
    next_flush_dt = next_flush_dt + perfs_interval;
    if(logger)
      logger->info("{} | PNL.NXT {}", Datetime(dt), Datetime(next_flush_dt));
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
