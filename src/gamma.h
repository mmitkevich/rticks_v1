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

template< typename TOutput=QuotesUpdated, 
          typename TObserver=void>
struct QuotingAlgo : public Algo, 
                     public Observable<TOutput, TObserver>, 
                     public Observer<QuotesUpdated>, 
                     public Observer<OrderFilled> 
{
  using Observable<TOutput,TObserver>::notify;

  NumericVector spread;       // spread to maintain  
  NumericVector offset;       // midspread directional offset 
  
  CharacterVector symbols;
  NumericVector mpi;
  BuySellVector market;  // market prices
  
  BuySellVector quotes;  // algorithm's bid & ask
  BuySellVector qty;     // algorithm's bid & ask qty
  
  NumericVector pos;     // current position
  
  QuotingAlgo(DataFrame params, List config) 
    : Algo(params, config),
      symbols(required<CharacterVector>(params, "symbol")),
      market(params.nrows()),
      quotes(params.nrows()),
      spread(required<NumericVector>(params, "spread")),
      offset(params.nrows())
  {  }

  int size() {
    return pos.size();
  }
  
  virtual void on_next(const QuotesUpdated &e) {
    market.update(e.symbol, e.quotes);
    notify();
  }
  
  virtual void on_next(const OrderFilled &e) {
    int side = e.side();
    int i = e.symbol.index;
    if((qty(side)[i] -= e.qty*side)==0)
      quotes(side)[i] = NAN;
    notify();
  }

  virtual void notify() {
    TOutput e;
    e.datetime = datetime;
    for(int i=0; i<size(); i++) {
      e.symbol = SymbolId(symbols[i], i);
      e.quotes = quotes.get(e.symbol);
      notify(e);
    }
  }
};

/// GammaAlgorithm holds a bunch of limit orders, at each minimal price step
/// qty at each level is exactly buy_gamma(t) for buys and sell_gamma(t) for sells.
/// distance between bid and ask is spread(t)

template<typename TObserver>
struct GammaAlgo : public QuotingAlgo<GammaQuotesUpdated, TObserver> {
  BuySellVector gamma;
  
  GammaAlgo(DataFrame params, List config)
    : QuotingAlgo<TObserver>(params, config),
      gamma(required<NumericVector>(params, "gamma.sell"),
            required<NumericVector>(params, "gamma.buy")) {
  }
  
  virtual bool notify(GammaQuotesUpdated &e) {
    e.gamma.buy = gamma.buy[e.symbol];
    e.gamma.sell = gamma.sell[e.symbol];
    return true;
  }
};

template< typename TOutput, 
          typename TObserver>
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
  
  virtual void on_next(const QuotesUpdated &e) {
    if(e.get_flag(Message::FROM_MARKET))
      market.update(e.symbol, e.quotes);
    else {
      // quotes from strategy
    }
  }
  
  // quotes from the gamma strategy  -  receive the gamma
  virtual void on_next(const GammaQuotesUpdated &e) {
    gamma.update(e.symbol, e.gamma);
    simulate(e.symbol);
  }
  
  virtual void simulate(const SymbolId &s) {
    if(market.buy[s] >= quotes.sell[s]) {
      // simulate the sells
      OrderFilled e;
      e.datetime = datetime;
      e.price = quotes.sell + 0.5*(market.buy[s]+quotes.sell);
      e.qty = - (gamma.sell[s] + truncl((market.buy[s]-quotes.sell[s])/mpi[s])*gamma.sell[s]);
      notify(e);
    }
    if(market.sell[s] <= quotes.buy[s]) {
      // simulate the buys
      OrderFilled e;
      e.datetime = datetime;
      e.price = quotes.sell + 0.5*(market.sell[s]+quotes.buy);
      e.qty =  (gamma.buy[s] + truncl((quotes.buy[s]-market.sell[s])/mpi[s])*gamma.buy[s]);
      notify(e);
    }
  }
};

}; //namespace Rcpp