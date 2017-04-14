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
    
    if(!m.count_buy())
      quote_buy(s, NAN);   // we don't quote when no bid price defined
    else if(!q.count_buy() || q.buy>m.buy)
        quote_buy(s, m.buy);  // we dont' quote better than market bid
    
    if(!m.count_sell())
      quote_sell(s, NAN);   // we don't quote when no ask price defined
    else if(!q.count_sell() || q.sell<m.sell)
        quote_sell(s, m.sell); // we don't quote better than market ask
  }

  void quote_buy(SymbolId s, double price) {
      price = round_price(s, price);
      double stop_price = stops.buy[s];      
      
      xlog<info>("ALGO.BID", s, price, gamma.buy[s]);
      
      if(pos[s]>-eps()) { // have long position or nothing
        price = std::min<double>(price, limits.buy[s]); // no entering longs above limit.buy
        if(price<stops.buy[s])
          price = -INFINITY;  // no long entries below buy stop price
      }else { // takeprofits for shorts
        price = std::min<double>(price, stops.sell[s] - spread[s]); // no closing shorts above stop.sell-spread
        stop_price = price - roundl(std::max<double>(-pos[s]-gamma.buy[s], 0.)/gamma.buy[s])*mpi[s];
      }
      place_order(s, gamma.buy[s], price, stop_price);
  }
  
  void quote_sell(SymbolId s, double price) {
    price = round_price(s, price);
    double stop_price = stops.sell[s];
  
    xlog<info>("ALGO.ASK", s, price, -gamma.sell[s]);
    
    if(pos[s]<eps()) { // have short position or nothing
      price = std::max<double>(price, limits.sell[s]); // no short-enters under sell limit price
      if(price>stops.sell[s])
        price = +INFINITY;  // no short entries above sell stop price
    }else { // take profits for longs
      price = std::max<double>(price, stops.buy[s]+spread[s]); // no closing longs below stop.buy+spread  
      stop_price = price + roundl(std::max<double>(pos[s] - gamma.buy[s], 0.) / gamma.buy[s])*mpi[s];
    }
    place_order(s, -gamma.sell[s], price, stop_price);
  }
  
  void place_order(SymbolId s, double qty, double price, double stop_price) {
    
    int side = qty>0?1:-1;
    
    if(std::isinf(price)) {
      price = stop_price = NAN;
    }
    
    if(!is_equal(quotes(side)[s],price) || !is_equal(stop_quotes(side)[s],stop_price)) 
    {
      quotes(side)[s] = price;
      stop_quotes(side)[s] = stop_price;
      
      TOrderMessage e;
      e.rtime = e.ctime = dt;
      e.symbol = s;
      //e.set_side(side); TODO: set_side is sign of qty!
      e.price = price;
      e.stop_price = stop_price;
      e.qty = qty;
      //FIXME: gamma -> $gamma input stream
      $orders.on_next(e);
    }
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
