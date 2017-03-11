#pragma once

#include "events.h"
#include "metrics.h"
#include "gammasim.h"

namespace Rcpp {

template<typename TAlgo,
         typename TMarket=GammaSimulator<typename TAlgo::order_message_type>>
struct Backtester : public Algo
{
    // from params
  NumericVector mpi;
  CharacterVector symbols;
  
  // from data
  NumericVector datetimes;
  NumericVector bids;
  NumericVector asks;
  NumericVector highs;
  NumericVector lows;
  CharacterVector virtual_symbol;

  int index;
  int stop; 

  TMarket market;
  TAlgo algo;

  Backtester(DataFrame params,     // symbol, mpi, spread, buy_gamma, sell_gamma
            List config)                // metrics_interval
    : Algo(params, config, "player"),
      market(params, config),
      algo(params, config),
      index(0),
      stop(0),
      mpi(required<NumericVector>(params, "mpi")),
      symbols(required<CharacterVector>(params, "symbol"))
  {
      algo.on_init(market);
  }
  
  SymbolId to_symbol_id(const char* sym) {
      for(int i=0;i<symbols.size();i++){
          if(symbols[i]==sym) {
              return SymbolId(sym, i);
          }
      }
      std::stringstream s;
      s << "symbol '" << sym << "' not found in params";
      Rcpp::stop(s.str());
  }

  void process(DataFrame data)    // datetime, symbol, bid, ask, high, low
  {
    datetimes = required<NumericVector>(data, "datetime");
    bids = required<NumericVector>(data, "bid");
    asks = required<NumericVector>(data, "ask");
    highs = optional<NumericVector>(data, "high");
    lows = optional<NumericVector>(data, "low");
    virtual_symbol = required<CharacterVector>(data, "virtual_id");
    
    logger->debug("rticks::Backtester::process {}", Datetime(datetimes[0]));
    
    index = 0;
    stop = data.nrows();
    double close_dt = datetimes[0];
    double open_dt = datetimes[0];
    dt = open_dt;
    while(index < stop) {
      dt = datetimes[index];
      if(dt<close_dt && !is_zero(dt-close_dt)) {
          std::stringstream s;
          s << "datetimes not sorted " << dt << ", " << close_dt <<", index=" << index;
          Rcpp::stop(s.str());
      }
      // buffer events
      QuoteMessage e;
      e.set_flag(Message::FROM_MARKET);
      e.set_side(OrderSide::BUY);
      e.symbol = to_symbol_id(virtual_symbol[index]);
      e.ctime = e.rtime = open_dt;
      e.price = bids[index];
      e.qty = +INFINITY;
      if(e.price!=0)
        market.on_next(e);  // send bid

      e.set_side(OrderSide::SELL);
      e.price = asks[index];
      e.qty = -INFINITY;
      if(e.price!=0)
        market.on_next(e);  // send ask

      market.notify(close_dt); // flush them

      open_dt = close_dt;
      close_dt = dt;
      index++;
    }
  }
};



}; //namespace Rcpp
