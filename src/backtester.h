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
    if(datetimes.size()==0)
        return;

    bids = required<NumericVector>(data, "bid");
    asks = required<NumericVector>(data, "ask");
    highs = optional<NumericVector>(data, "high");
    lows = optional<NumericVector>(data, "low");
    virtual_symbol = required<CharacterVector>(data, "virtual_id");
    
    logger->debug("rticks::Backtester::process {}", Datetime(datetimes[0]));
    
    index = 0;
    stop = data.nrows();
    dt = datetimes[0];
    double close_dt = dt;
    int sent_events=0;
    while(index < stop) {
      dt = datetimes[index];
      if(dt<close_dt && !is_zero(dt-close_dt)) {
          std::stringstream s;
          s << "datetimes not sorted " << dt << ", " << close_dt <<", index=" << index;
          Rcpp::stop(s.str());
      }
      if(bids[index]>=asks[index]-eps()) {
        std::stringstream s;
        s << "bid="<<bids[index]<<" >= ask="<<asks[index]<<" " << dt << ", " << close_dt <<", index=" << index;
        Rcpp::stop(s.str());
      }
      
      auto s = to_symbol_id(virtual_symbol[index]); //TODO: fix symbol search via hashmap
      double time2 = dt + 1e-6;
      if(s.index>=0) {
        // buffer events
        QuoteMessage bid;
        bid.set_flag(Message::FROM_MARKET);
        bid.set_side(OrderSide::BUY);
        bid.symbol = s;
        bid.ctime = bid.rtime = dt;
        bid.price = bids[index];
        bid.qty = +INFINITY;
        
        QuoteMessage ask;
        ask.set_flag(Message::FROM_MARKET);
        ask.set_side(OrderSide::SELL);
        ask.symbol = s;
        ask.ctime = ask.rtime = dt;
        ask.price = asks[index];
        ask.qty = -INFINITY;
        
        if(index>0 && bid.price>=asks[index-1]-eps()) {
          // this is more realistic sequence
          market.on_next(ask);
          bid.ctime = bid.rtime = time2; // make bid later
          market.on_next(bid);
          sent_events++;
        }else {
          market.on_next(bid);  // send bid
          ask.ctime = ask.rtime = time2; // make ask later
          market.on_next(ask);  // send ask
          sent_events++;
        }
      }
      
      market.notify(time2+1e-5); // flush the time

      close_dt = dt;
      index++;
    }
  }
};



}; //namespace Rcpp
