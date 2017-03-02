#pragma once

#include "events.h"
#include "metrics.h"

namespace Rcpp {

// Observable<QuotesUpdated>
template<typename TOutput=QuotesUpdated, 
         typename TObserver=Observer<TOutput>,
         typename TBase=Processor<DataFrame, TOutput, TObserver> >
struct Player : public Algo, 
                public TBase
{
  using TBase::notify;

  
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
  

  Player(DataFrame params,     // symbol, mpi, spread, buy_gamma, sell_gamma
            List config)                // metrics_interval
    : Algo(params, config),
      index(0),
      stop(0),
      mpi(required<NumericVector>(params, "mpi")),
      symbols(required<CharacterVector>(params, "symbol"))
      {  }
  
  double now() {
    return datetimes[index];
  }

  int nsym() {
      return params.size();
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

  void on_next(DataFrame data)    // datetime, symbol, bid, ask, high, low
  {
    datetimes = required<NumericVector>(data, "datetime");
    bids = required<NumericVector>(data, "bid");
    asks = required<NumericVector>(data, "ask");
    highs = optional<NumericVector>(data, "high");
    lows = optional<NumericVector>(data, "low");
    virtual_symbol = required<CharacterVector>(data, "virtual_id");

    index = 0;
    stop = data.nrows();
    std::deque<TOutput> ba; // bid & ask
    std::deque<TOutput> hl; // high & low
    double close_dt = datetimes[0];
    double open_dt = close_dt; // TODO: close_dt -1
    while(index < stop) {
      double dt = datetimes[index];
      // TODO: notify high-low events before bid-ask
      // for(int i=0; i<hl.size(); i++)
      //    notify(hl[i]);
      if(dt<close_dt) {
          std::stringstream s;
          s << "datetimes not sorted " << dt << ", " << close_dt;
          Rcpp::stop(s.str());
      }else if(!is_zero(dt-close_dt)) {
          while(ba.size()>0) {
              notify(ba.front());
              ba.pop_front();
          }
      }
      // buffer events
      TOutput e;
      e.set_flag(Message::FROM_MARKET);
      e.symbol = to_symbol_id(virtual_symbol[index]);
      e.datetime = open_dt;
      e.quotes.buy = bids[index];
      e.quotes.sell = asks[index];
      ba.push_back(e);
      //e.set_flag(QuotesUpdated::HIGH_LOW);
      //hl.push_back(e);
      std::cout << std::flush;
      std::cout << std::tuple(Datetime(dt), e.symbol, ;

      open_dt = close_dt;
      close_dt = datetimes[index];
      index++;
    }
  }
};



}; //namespace Rcpp
