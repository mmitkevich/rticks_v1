#pragma once
#ifndef GAMMASIM_H
#define GAMMASIM_H

#include "utils.h"

namespace Rcpp {

template<typename TOrderMessage=OrderMessage,
         typename TQuoteMessage=QuoteMessage,
         typename TExecutionMessage=ExecutionMessage,
         typename TSessionMessage=SessionMessage,
         typename TValueMessage=ValueMessage<double>,
         typename TMetrics=Metrics<TExecutionMessage>,
         typename TScheduler=Scheduler<Algo> >
struct GammaSimulator : public MarketAlgo,
                        // inputs
                        public IObserver<TQuoteMessage>,         // from market
                        public IObserver<TOrderMessage>,        // from algo
                        public IObserver<TSessionMessage>,
                        public IObserver<TValueMessage>
// from market

{
  typedef TOrderMessage order_message_type;

  BuySellVector gamma;    // gamma

  
  // outputs
  LatencyQueue<TSessionMessage> $session;
  LatencyQueue<TQuoteMessage> $quotes;
  LatencyQueue<TExecutionMessage> $execs;
  LatencyQueue<TOrderMessage> $orders;
  LatencyQueue<TValueMessage> $params;
  
  TScheduler scheduler;
  TMetrics metrics;

  double check_big_qty;
  
  GammaSimulator(DataFrame params, List config, std::string name="gammasim")
    : MarketAlgo(params, config, name),
      scheduler(params, config),
      metrics(params, config),
      $session(params, config, &scheduler, "$sess  "),
      $quotes(params, config, &scheduler, "$quotes "),
      $execs(params, config, &scheduler,  "$execs  "),
      $orders(params, config, &scheduler, "$orders "),
      $params(params, config, &scheduler, "$params "),
      check_big_qty(optional<NumericVector>(config,"check_big_qty",1e6)[0])
  {
      //quotes.buy = quotes.sell = NAN; // FIXME: this is wrong. quotes.buy and quotes.sell will be SHARED NumVector.
      // TODO: migrate to std::vector instead of NumericVector???
      $orders >>= *this;
      // wire up metrics
      metrics.on_init(*this);
      
      //log_level = 1;
  }
  
  virtual void on_next(TQuoteMessage e) {
    //bool is_session_start = std::isnan(datetime());
    on_clock(e.rtime);
    
    //if(is_session_start) {
    //   on_session_open(e.rtime);
    //}
    auto s = e.symbol;
    dlog<debug>(e);
    assert(e.flag(Message::FROM_MARKET));
    market.update(s, e);
    xlog<debug>("SIM.QUOT", s);
    on_simulate(e.symbol);
    // forward to algorithms
    $quotes.on_next(std::move(e));
  }

  virtual void on_next(TValueMessage e) {
    $params.on_next(std::move(e));
  }

  // quotes from the gamma strategy  -  receive the gamma
  virtual void on_next(TOrderMessage e) {
    on_clock(e.rtime);
    dlog<debug>(e);
    assert(!std::isnan(e.price));
    assert(!std::isnan(e.qty));
    gamma(e.side())[e.symbol] = fabs(e.qty);
    stop_quotes(e.side())[e.symbol] = e.stop_price;
    auto old_quote = quotes(e.side())[e.symbol];
    quotes(e.side())[e.symbol] = e.price;
    xlog<info>("SIM.MOV", e.symbol, old_quote, e.qty);
    on_simulate(e.symbol);
  }

  virtual void on_next(TSessionMessage e) {
    $session.on_next(e);
  }
  
  bool should_fill(double overlap) {
    return overlap >= - eps();  // equal with rounding error or greater
  }
  
  virtual void on_simulate(const SymbolId& s, bool aggressive=false) {
    ExecutionMessage e;
    e.set_flag(ExecutionMessage::IS_FILL);
    e.ctime = e.rtime = dt;
    e.symbol = s;
    e.qty_leaves = 0;
    auto m = market[s];
    auto q = quotes[s];
    auto pi = mpi[s];
    auto g = gamma[s];
    double slippage = 10; // mpi
    assert(!std::isnan(pi));
    if(!std::isnan(q.sell) && should_fill(m.buy-q.sell)) {
      // simulate the sells
      auto stop = m.buy;
      if(!std::isnan(stop_quotes.sell[s]))
        stop = std::min(stop, stop_quotes.sell[s]);
      
      e.price = stop; //m.buy
      e.fill_price = aggressive ? m.buy - slippage*pi : 0.5*(stop+q.sell);
      e.qty = - (g.sell + roundl((stop-q.sell)/pi)*g.sell);
      pos[s] +=e.qty;
      xlog<info>("SIM.SELL", s, e.fill_price, e.qty);
      // move up sell quote since it got filled
      market.buy[s] = quotes.sell[s] - pi;  // for new quotes market spread should widen or infinite loop occurs
      quotes.sell[s] = m.buy + pi;

      if(!std::isnan(stop_quotes.sell[s]) && quotes.sell[s]>stop_quotes.sell[s]+eps()) {
        quotes.sell[s] = NAN;
        stop_quotes.sell[s] = NAN;
      }
      check(e);
      $execs.on_next(e);
    }
    if(!std::isnan(q.buy) && should_fill(-(m.sell-q.buy))) {
      // simulate the buys
      auto stop = m.sell;
      if(!std::isnan(stop_quotes.buy[s]))
        stop = std::max(stop, stop_quotes.buy[s]);
      
      e.price = stop; // m.sell
      e.fill_price = aggressive ? m.sell + slippage*pi : 0.5*(stop+q.buy);
      e.qty =  (g.buy + roundl((q.buy-stop)/pi)*g.buy);
      pos[s] +=e.qty;
      
      xlog<info>("SIM.BUY ", s, e.fill_price, e.qty);

      market.sell[s] = quotes.buy[s] + pi;  // for new quotes market spread should widen or infinite loop occurs
      // move down buy quote since it got filled
      quotes.buy[s] = m.sell - pi;
      
      if(!std::isnan(stop_quotes.buy[s]) && quotes.buy[s]<stop_quotes.buy[s]-eps()) {
        quotes.buy[s] = NAN;
        stop_quotes.buy[s] = NAN;
      }
      check(e);
      $execs.on_next(e);
    }
  }

  void check(const ExecutionMessage &e) {
      if(fabs(e.qty)>=check_big_qty) {
        auto s = e.symbol;
        xlog<warn>("BIG.QTY!", e.symbol, e.fill_price, e.qty);
      }
  }
  
  void notify(double dt)  {
    scheduler.on_execute(dt);
  }
};

}; // ns Rcpp
#endif // GAMMASIM_H

