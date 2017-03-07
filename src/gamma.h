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
      limits( optional<NumericVector>(params, "buy", +INFINITY),
              optional<NumericVector>(params, "sell", -INFINITY)),
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

  double midprice(const BuySell &m, double mpi) {
      if(m.count_buy() && m.count_sell())
        return 0.5 * (m.buy + m.sell);
      else
        assert(false);
      return NAN;
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
    auto m = market[s];
    auto pi = mpi[s];
    // restore our quotes if needed
    auto q = quotes[s];
    if(!q.count_buy() && !q.count_sell()) { // no buy & no sell
        auto mid = midprice(m, pi);
        if(!std::isnan(mid)) {
            quotes.buy[s] = round_price(s, mid - 0.5 * spread[s]);
            xlog<1>("ALGO.BID", s, quotes[s], m);
            notify(s, OrderSide::BUY);
            quotes.sell[s] = round_price(s, mid + 0.5 * spread[s]);
            xlog<1>("ALGO.ASK", s, quotes[s], m);
            notify(s, OrderSide::SELL);
        }
    }else if(!q.count_buy()) {  // no buy
        quotes.buy[s] = q.sell - spread[s];
        xlog<1>("ALGO.BID", s, quotes[s], m);
        notify(s, OrderSide::BUY);
    }else if(!q.count_sell()) { // no sell
        quotes.sell[s] = q.buy + spread[s];
        xlog<1>("ALGO.ASK", s, quotes[s], m);
        notify(s, OrderSide::SELL);
    }
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
        quotes(side)[s] = e.price - mpi[s]*side;      // side>0 => we bought at buy => move down
        //qty(side)[s] = gamma(side)[s];                // restore qty
        xlog<1>(side>0?"ALG.XBID":"ALG.XASK", s, quotes[s], market[s], pos[s]);
        notify(e.symbol, side);
        // move opposite site
        quotes(-side)[s] = e.price + spread[s]*side - mpi[s]*side;
        //qty(-side)[s] = gamma(side)[s];               // restore qty
        xlog<1>(-side>0?"ALG.XBID":"ALG.XASK", s, quotes[s], market[s], pos[s]);
        notify(e.symbol, -side);
    }
  }

  // notify on quotes change
  virtual void notify(SymbolId s, int side) {
    
    if(pos[s]>=0 && quotes.buy[s]>limits.buy[s])
      quotes.buy[s] = NAN;
    if(pos[s]<=0 && quotes.sell[s]<limits.sell[s])
      quotes.sell[s] = NAN;

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
