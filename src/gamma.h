#pragma once

#include "utils.h"
#include "events.h"
#include "simulator.h"
#include "metrics.h"

namespace Rcpp {

/// Algorithm is a function of its inputs
/// inputs are overloads 
///   output_type operator()(T &)
/// after processing the input, all overloads call
///   operator() without parameters so some post-action could be applied

/// GammaAlgorithm holds a bunch of limit orders, at each minimal price step
/// qty at each level is exactly buy_gamma(t) for buys and sell_gamma(t) for sells.
/// distance between bid and ask is spread(t)

template< typename TOutput=GammaQuotesUpdated,
          typename TObserver=Observer<TOutput> >
struct QuotingAlgo : public Algo,
                     public Observable<TOutput, TObserver>,
                     public Observer<QuotesUpdated>,
                     public Observer<OrderFilled>
{
  using Observable<TOutput,TObserver>::notify;

  /// parameters
  NumericVector spread;       // spread to maintain  
  NumericVector offset;       // midspread directional offset 
  BuySellVector gamma;        // qty to quote on buy/sell sides

  /// from parameters also
  CharacterVector symbols;
  NumericVector mpi;

  /// inputs
  BuySellVector market;  // current market prices
  NumericVector pos;     // current position

  /// outputs
  BuySellVector quotes;  // algorithm's bid & ask
  BuySellVector qty;     // algorithm's best bid & ask qty unfilled

  QuotingAlgo(DataFrame params, List config) 
    : Algo(params, config),
      symbols(required<CharacterVector>(params, "symbol")),
      market(params.nrows()),
      quotes(params.nrows()),
      spread(required<NumericVector>(params, "spread")),
      offset(params.nrows())
  {  }

  int size() {
    return symbols.size();
  }
  
  virtual void on_next(QuotesUpdated e) {
    market.update(e.symbol, e.quotes);
  }
  
  virtual void on_next(OrderFilled e) {
    int side = e.side();
    if((qty(side)[e.symbol] -= e.qty*side)==0)
        on_filled(e.symbol, side);
  }

  // full fill - move the spread
  virtual void on_filled(SymbolId s, int side) {
    quotes(side)[s] -= mpi[s]*side; // side>0 => we bought at buy => move down
    qty(side)[s] = gamma(side)[s];     // restore liquidity
    notify(s);
  }

  // notify on quotes change
  virtual void notify(SymbolId symbol) {
    TOutput e;
    e.datetime = datetime;
    e.symbol = symbol;
    e.quotes = quotes[e.symbol];
    notify(e);
  }
};


template< typename TOutput=OrderFilled,
          typename TObserver=Observer<TOutput> >
struct GammaSimulator : public Algo, 
                        public Observable<TOutput, TObserver>, 
                        public Observer<QuotesUpdated>,
                        public Observer<GammaQuotesUpdated>
{
  using Observable<TOutput, TObserver>::notify;
  
  BuySellVector market;   // market prices
  BuySellVector gamma;    // gamma
  BuySellVector quotes;   // algorithm quotes
  NumericVector mpi;      // min price step
  
  GammaSimulator(DataFrame params, List config) 
    : Algo(params, config) { 
  }
  
  virtual void on_next(QuotesUpdated e) {
    if(e.flag(Message::FROM_MARKET))
      market.update(e.symbol, e.quotes);
    else {
      // quotes from strategy
    }
    on_simulate(e.symbol);
  }
  
  // quotes from the gamma strategy  -  receive the gamma
  virtual void on_next(GammaQuotesUpdated e) {
    gamma.update(e.symbol, e.gamma);
    on_simulate(e.symbol);
  }
  
  virtual void on_simulate(SymbolId s) {
    OrderFilled e;
    e.symbol = s;
    e.datetime = datetime;
    auto m = market[s];
    auto q = quotes[s];
    auto pi = mpi[s];
    auto g = gamma[s];
    if(m.buy >= q.sell) {
      // simulate the sells
      e.price = q.sell + 0.5*(m.buy+q.sell);
      e.qty = - (g.sell + truncl((m.buy-q.sell)/pi)*g.sell);
      notify(e);
    }
    if(m.sell <= q.buy) {
      // simulate the buys
      e.price = q.sell + 0.5*(m.sell+q.buy);
      e.qty =  (g.buy + truncl((q.buy-m.sell)/pi)*g.buy);
      notify(e);
    }
  }
};

}; //namespace Rcpp
