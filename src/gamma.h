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

template<typename TOrderMessage = OrderMessage>
struct GammaAlgo : public MarketAlgo,
                     // inputs -> on_next(T)
                     public IObserver<QuoteMessage>,
                     public IObserver<ExecutionMessage>
{
  typedef TOrderMessage order_message_type;

  /// parameters
  NumericVector spread;       // spread to maintain  
  //NumericVector offset;       // midspread directional offset
  BuySellVector gamma;        // qty to quote on buy/sell sides
  BuySellVector limits;
  /// state
  //BuySellVector qty;     // latest our bid & ask outstanding qty (not filled as far as we could know)

  /// output
  Stream<TOrderMessage> $orders;

  GammaAlgo(DataFrame params, List config, std::string name="gammalgo")
    : MarketAlgo(params, config, name),
      limits( required<NumericVector>(params, "buy"),
              required<NumericVector>(params, "sell")),
      gamma(required<NumericVector>(params, "gamma.buy"),
            required<NumericVector>(params, "gamma.sell")),
      spread(required<NumericVector>(params, "spread"))
      //offset(params.nrows(), NAN)
  {  }

  int size() {
    return symbols.size();
  }
  
  template<typename TContext>
  void on_init(TContext &ctx) {
      // wire up algo
      ctx.$quotes >>= *this;
      ctx.$execs >>= *this;
      $orders >>= ctx.$orders;
  }


  double round_price(SymbolId s, double price) {
      return roundl(price/mpi[s])*mpi[s];
  }

  virtual void on_next(QuoteMessage e) {
    on_clock(e.rtime);
    dlog<3>(e);
    auto s = e.symbol;
    auto side = e.side();
    // update cached market price
    market.update(s, e);
    // restore our quotes if needed
    auto q = quotes[s];
    if(!q.count_buy() && !q.count_sell()) { // no buy & no sell
        auto mid = market.midprice(s);
        if(!std::isnan(mid)) {
            quote_buy(s, mid - 0.5 * spread[s]);
            quote_sell(s, mid + 0.5 * spread[s]);
        }
    }else if(!q.count_buy()) {  // no buy
        quote_buy(s, q.sell - spread[s]);
    }else if(!q.count_sell()) { // no sell
        quote_sell(s, q.buy + spread[s]);
    }
  }

  bool quote_buy(SymbolId s, double price, double fill_qty=NAN) {
      price = round_price(s, price);
      if(pos[s]>=0 && price>limits.buy[s])
        price = NAN;
      if(!is_equal(quotes.buy[s], price)) {
        quotes.buy[s] = price;
        xlog<1>("ALGO.BID", s, quotes[s], market[s], pos[s], fill_qty);
        notify(s, OrderSide::BUY);
        return true;
      }
      return false;
  }

  bool quote_sell(SymbolId s, double price, double fill_qty=NAN) {
      price = round_price(s, price);
      if(pos[s]<=0 && price<limits.sell[s])
        price = NAN;
      if(!is_equal(quotes.sell[s],price)){
        quotes.sell[s] = price;
        xlog<1>("ALGO.ASK", s, quotes[s], market[s], pos[s]);
        notify(s, OrderSide::SELL);
        return true;
      }
      return false;
  }

  virtual void on_next(ExecutionMessage e) {
    on_clock(e.rtime);
    dlog<3>(e);
    auto side = e.side();
    auto s = e.symbol;

    //qty(side) = e.qty_leaves;   // update qty left

    if(e.is_full_fill()) {      // full fill
        pos[s] += e.qty;        // track the position
        // move this side
        if(side>0) {
            quote_buy(s, e.price-mpi[s],  e.qty);
            quote_sell(s, e.price-mpi[s]+spread[s], e.qty);
        } else {
            quote_sell(s, e.price+mpi[s],  e.qty);
            quote_buy(s, e.price+mpi[s]-spread[s], e.qty);
        }
    }
  }

  // notify on quotes change
  virtual void notify(SymbolId s, int side) {
    TOrderMessage e;
    e.rtime = e.ctime = dt;
    e.symbol = s;
    //e.set_side(side); TODO: set_side is sign of qty!
    e.price = quotes(side)[e.symbol];
    e.qty = gamma(side)[e.symbol]*side;
    $orders.on_next(e);
  }
};



}; //namespace Rcpp
