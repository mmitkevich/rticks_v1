#pragma once

// should be already included
//#include "simulator.h"

namespace Rcpp {

/// Algorithm is a function of its inputs
/// inputs are overloads 
///   output_type operator()(T &)
/// after processing the input, all overloads call
///   operator() without parameters so some post-action could be applied

/// GammaAlgorithm holds a bunch of limit orders, at each minimal price step
/// qty at each level is exactly buy_gamma(t) for buys and sell_gamma(t) for sells.
/// distance between bid and ask is spread(t)

template<typename TOrderMessage = GammaMessage>
struct GammaAlgo : public MarketAlgo,
                     // inputs -> on_next(T)
                     public IObserver<QuoteMessage>,
                     public IObserver<ExecutionMessage>
{
  /// parameters
  NumericVector spread;       // spread to maintain  
  //NumericVector offset;       // midspread directional offset
  BuySellVector gamma;        // qty to quote on buy/sell sides

  /// from parameters also
  CharacterVector symbols;
  NumericVector mpi;

  /// state
  BuySellVector market;  // latest market prices
  NumericVector pos;     // latest position
  BuySellVector quotes;  // latest our bid & ask
  BuySellVector qty;     // latest our bid & ask outstanding qty (not filled as far as we could know)

  /// output
  Stream<TOrderMessage> $orders;

  GammaAlgo(DataFrame params, List config)
    : MarketAlgo(params, config),
      symbols(required<CharacterVector>(params, "symbol")),
      market(params.nrows()),
      quotes(params.nrows()),
      gamma(required<NumericVector>(params, "gamma.buy"),
            required<NumericVector>(params, "gamma.sell")),
      spread(required<NumericVector>(params, "spread"))
      //offset(params.nrows(), NAN)
  {  }

  int size() {
    return symbols.size();
  }
  
  virtual double midprice(const BuySell &m) {
      if(m.count_buy() && m.count_sell())
        return 0.5 * (m.buy + m.sell);
      else
        assert(false);
  }

  virtual void on_next(QuoteMessage e) {
    auto s = e.symbol;
    auto side = e.side();
    // update cached market price
    market.update(s, e);
    auto m = market[s];

    // restore our quotes if needed
    auto q = quotes[s];
    if(!q.count_buy() && !q.count_sell()) { // no buy & no sell
        auto mid = midprice(m);
        quotes.buy[s] = mid - 0.5 * spread[s];
        notify(s, OrderSide::BUY);
        quotes.sell[s] = mid + 0.5 * spread[s];
        notify(s, OrderSide::SELL);
    }else if(!q.count_buy()) {  // no buy
        quotes.buy[s] = q.sell - spread[s];
        notify(s, OrderSide::BUY);
    }else if(!q.count_sell()) { // no sell
        quotes.sell[s] = q.buy + spread[s];
        notify(s, OrderSide::SELL);
    }
  }
  
  virtual void on_next(ExecutionMessage e) {
    auto side = e.side();
    auto s = e.symbol;

    qty(side) = e.qty_leaves;   // update qty left

    if(e.is_full_fill()) {      // full fill
        // move this side
        quotes(side)[s] = e.price - mpi[s]*side;      // side>0 => we bought at buy => move down
        qty(side)[s] = gamma(side)[s];                // restore qty
        notify(e.symbol, side);

        // move opposite site
        quotes(-side)[s] = e.price + spread[s] - mpi[s]*side;
        qty(side)[s] = gamma(side)[s];               // restore qty
        notify(e.symbol, -side);
    }
  }

  // notify on quotes change
  virtual void notify(SymbolId symbol, int side) {
    TOrderMessage e;
    e.datetime = dt;
    e.symbol = symbol;
    e.set_side(side);
    e.price = quotes(side)[e.symbol];
    e.gamma = gamma(side)[e.symbol];
    $orders.notify(e);
  }
};


template<typename TOrderMessage=GammaMessage,
         typename TQuoteMessage=QuoteMessage,
         typename TExecutionMessage=ExecutionMessage>
struct GammaSimulator : public MarketAlgo,
                        // inputs
                        public IObserver<TQuoteMessage>,         // from market
                        public IObserver<TOrderMessage>          // from algo
{
  BuySellVector market;   // market prices
  BuySellVector gamma;    // gamma
  BuySellVector quotes;   // algorithm quotes
  NumericVector mpi;      // min price step

  // outputs
  Stream<TQuoteMessage> $quotes;
  Stream<TExecutionMessage> $execs;

  GammaSimulator(DataFrame params, List config) 
    : MarketAlgo(params, config),
      mpi(required<NumericVector>(params, "mpi"))
  {
      //quotes.buy = quotes.sell = NAN; // FIXME: this is wrong. quotes.buy and quotes.sell will be SHARED NumVector.
      // TODO: migrate to std::vector instead of NumericVector???
  }
  
  virtual void on_next(TQuoteMessage e) {
    assert(e.flag(Message::FROM_MARKET));
    market.update(e.symbol, e);
    on_simulate(e.symbol);
    // forward to algorithms
    $quotes(std::move(e));
  }
  
  // quotes from the gamma strategy  -  receive the gamma
  virtual void on_next(TOrderMessage e) {
    assert(!std::isnan(e.price));
    assert(!std::isnan(e.gamma));
    gamma(e.side())[e.symbol] = e.gamma;
    quotes(e.side())[e.symbol] = e.price;
    on_simulate(e.symbol);
  }
  
  virtual void on_simulate(const SymbolId& s) {
    ExecutionMessage e;
    e.datetime = dt;
    e.symbol = s;
    auto m = market[s];
    auto q = quotes[s];
    auto pi = mpi[s];
    auto g = gamma[s];
    assert(!std::isnan(pi));
    if(!std::isnan(q.sell) && m.buy >= q.sell) {
      // simulate the sells
      e.price = 0.5*(m.buy+q.sell);
      e.qty = - (g.sell + truncl((m.buy-q.sell)/pi)*g.sell);
      $execs.notify(e);
    }
    if(!std::isnan(q.buy) && m.sell <= q.buy) {
      // simulate the buys
      e.price = 0.5*(m.sell+q.buy);
      e.qty =  (g.buy + truncl((q.buy-m.sell)/pi)*g.buy);
      $execs.notify(e);
    }
  }
};

}; //namespace Rcpp
