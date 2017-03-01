#pragma once

#include "events.h"
#include "metrics.h"

namespace Rcpp {

// Observable<QuotesUpdated>
template<typename TOutput=QuotesUpdated, 
         typename TObserver=Observer<TOutput>,
         typename TObservable=Observable<TOutput, TObserver> >
struct Player : public Algo, 
                public TObservable,
                public Observer<List>
{
  using TObservable::notify;

  typedef RowWrapper<DataFrame, Player, double, NumericVector> DataRow;
  
    // from params
  NumericVector mpi;
  CharacterVector symbols;
  
  // from data
  NumericVector datetimes;
  DataRow df_bid;
  DataRow df_ask;
  DataRow df_high;
  DataRow df_low;
  
  int index;
  int stop; 
  
  Player(DataFrame params,     // symbol, mpi, spread, buy_gamma, sell_gamma
            List config)                // metrics_interval
    : Algo(params, config),
      index(0),
      stop(0),
      mpi(required<NumericVector>(params, "mpi")),
      df_bid(this),
      df_ask(this),
      df_high(this),
      df_low(this) {  }
  
  double now() {
    return datetimes[index];
  }
  
  double bid(SymbolId sym) {
    return df_bid[sym.index][index];
  }
  
  double ask(SymbolId sym) {
    return df_ask[sym.index][index];
  }
  
  void on_next(List data)    // datetime, bid, ask, high, low
  {
    datetimes = required<NumericVector>(data, "datetime");
    df_bid.data = required<DataFrame>(data, "bid");
    df_ask.data = required<DataFrame>(data, "ask");
    df_high.data = required<DataFrame>(data, "high");
    df_low.data = required<DataFrame>(data, "low");
    
    index = 0;
    stop = data.size();
    int nsym = df_bid.size();
    while(index < stop) {
      int isym;
      // feed market quotes (sell = lowest(sell), buy = highest(buy)) into simulator to work out fills upto time t
      for(isym = 0; isym < nsym; isym++) {
        TOutput e; 
        e.set_flag(Message::FROM_MARKET | QuotesUpdated::HIGH_LOW);
        e.datetime = index > 0 ? datetimes[index-1] : NAN; // previous time
        e.symbol = SymbolId(symbols[isym], isym);
        e.datetime = now();
        e.quotes.buy = df_high[isym];
        e.quotes.sell = df_low[isym];
        notify(e);
      }
      for(isym = 0; isym < nsym; isym++) {
        TOutput e;
        e.set_flag(Message::FROM_MARKET);
        e.symbol = SymbolId(symbols[isym], isym);
        e.datetime = now();
        e.quotes.buy = df_bid[isym];
        e.quotes.sell = df_ask[isym];
        notify(e);
      }
      index++;
    }
  }
};



}; //namespace Rcpp
