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
  BuySellVector stops;
  
  /// state
  //BuySellVector qty;     // latest our bid & ask outstanding qty (not filled as far as we could know)

  /// output
  Stream<TOrderMessage> $orders;

  GammaAlgo(DataFrame params, List config, std::string name="gammalgo")
    : MarketAlgo(params, config, name),
      limits( required<NumericVector>(params, "limit.buy"),
              required<NumericVector>(params, "limit.sell")),
      stops( required<NumericVector>(params, "stop.buy"),
              required<NumericVector>(params, "stop.sell")),
      gamma(required<NumericVector>(params, "gamma.buy"),
            required<NumericVector>(params, "gamma.sell")),
      spread(required<NumericVector>(params, "spread"))
      //offset(params.nrows(), NAN)
  {  }

  int size() {
    return symbols.size();
  }
  
  template<typename TMarket>
  void on_init(TMarket &mkt) {
      // wire up algo
      mkt.$quotes >>= *this;
      mkt.$execs >>= *this;
      $orders >>= mkt.$orders;
  }


  double round_price(SymbolId s, double price) {
      return roundl(price/mpi[s])*mpi[s];
  }

  virtual void on_next(QuoteMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    auto s = e.symbol;
    auto side = e.side();
    // update cached market price
    market.update(s, e);
    auto m = market[s];
    // restore our quotes if needed
    auto q = quotes[s];
    if(!q.count_buy() && !q.count_sell()) { // no buy & no sell
        auto mid = market.midprice(s);
        if(!std::isnan(mid)) {
            quote_buy(s, mid - 0.5 * spread[s]);
            quote_sell(s, mid + 0.5 * spread[s]);
        }
    }else if(!q.count_buy()) {  // no buy
        // check the sell - could be too far from market
        if(q.sell-m.buy > spread[s]) {
          quote_sell(s, m.buy + spread[s]);
        }
        quote_buy(s, quotes.sell[s] - spread[s]);
    }else if(!q.count_sell()) { // no sell
        // check the buy - could be too far from market
        if(m.sell - q.buy > spread[s]) {
          quote_buy(s, m.sell - spread[s]);
        }
        quote_sell(s, quotes.buy[s] + spread[s]);
    }
  }

  bool quote_buy(SymbolId s, double price) {
      price = round_price(s, price);
      auto stop_price = stops.buy[s]; // no entering longs under stop.buy
        
      price = std::min<double>(price, limits.buy[s]); // no entering longs above limit.buy
      if(price<stops.buy[s])
        price = NAN;  // no long entries below buy stop price
      else if(price>stops.sell[s]) 
        price = stops.sell[s];  // no closing shorts above stop.sell
      if(std::isinf(price))
          price = NAN;

      stop_price = price - roundl(std::max<double>(-pos[s]-gamma.buy[s], 0.)/gamma.buy[s])*mpi[s];
      stop_price = std::min<double>(stop_price, stops.sell[s]); // no closing shorts above stop.sell


      quotes.buy[s] = price;
      
      xlog<info>("ALGO.BID", s, quotes[s], market[s], pos[s], gamma.buy[s]);

      TOrderMessage e;
      e.rtime = e.ctime = dt;
      e.symbol = s;
      //e.set_side(side); TODO: set_side is sign of qty!
      e.price = price;
      e.stop_price = stop_price;
      e.qty = gamma.buy[e.symbol];
      //FIXME: gamma -> $gamma input stream
      $orders.on_next(e);
      return true;
  }

  bool quote_sell(SymbolId s, double price) {
      price = round_price(s, price);

      auto stop_price = stops.sell[s]; // no shorts above stop price
      
      // opening shorts      
      price = std::max<double>(price, limits.sell[s]); // no short-enters under sell limit price
      if(price>stops.sell[s])
        price = NAN;  // no short entries above sell stop price
      else if(price<stops.buy[s])
        price = stops.buy[s]; // no closing long under stop.buy
      if(std::isinf(price))
          price = NAN;

      stop_price = price + roundl(std::max<double>(pos[s] - gamma.buy[s], 0.) / gamma.buy[s])*mpi[s];
      stop_price = std::max<double>(stop_price, stops.buy[s]); // no closing longs below stop.buy

      quotes.sell[s] = price;
      
      xlog<info>("ALGO.ASK", s, quotes[s], market[s], pos[s], gamma.sell[s]);
      TOrderMessage e;
      e.rtime = e.ctime = dt;
      e.symbol = s;
      //e.set_side(side); TODO: set_side is sign of qty!
      e.price = price;
      e.stop_price = stop_price;
      e.qty = -gamma.sell[e.symbol];
      $orders.on_next(e);
      return true;
  }

  virtual void on_next(ExecutionMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    auto side = e.side();
    auto s = e.symbol;

    //qty(side) = e.qty_leaves;   // update qty left

    if(e.is_full_fill()) {      // full fill
        pos[s] += e.qty;        // track the position
        // move this side
        if(side>0) {
            quote_buy(s, e.price-mpi[s]);
            quote_sell(s, e.price+spread[s]); //-mpi[s]
        } else {
            quote_sell(s, e.price+mpi[s]);
            quote_buy(s, e.price-spread[s]); //+mpi[s]
        }
    }
  }
};



}; //namespace Rcpp
