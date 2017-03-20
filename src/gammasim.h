#pragma once
#ifndef GAMMASIM_H
#define GAMMASIM_H

#include "utils.h"

namespace Rcpp {

template<typename TOrderMessage=OrderMessage,
         typename TQuoteMessage=QuoteMessage,
         typename TExecutionMessage=ExecutionMessage,
         typename TSessionMessage=SessionMessage,
         typename TMetrics=Metrics<TExecutionMessage>,
         typename TScheduler=Scheduler<Algo> >
struct GammaSimulator : public MarketAlgo,
                        // inputs
                        public IObserver<TQuoteMessage>,         // from market
                        public IObserver<TOrderMessage>          // from algo
{
  typedef TOrderMessage order_message_type;

  BuySellVector gamma;    // gamma

  // outputs
  LatencyQueue<TSessionMessage> $session;
  LatencyQueue<TQuoteMessage> $quotes;
  LatencyQueue<TExecutionMessage> $execs;
  LatencyQueue<TOrderMessage> $orders;
  
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
    if(std::isnan(datetime())) {
       on_session_start(e.rtime);
    }
    on_clock(e.rtime);

    dlog<info>(e);
    assert(e.flag(Message::FROM_MARKET));
    market.update(e.symbol, e);
    xlog<debug>("SIM.QUOT", e.symbol, quotes[e.symbol], market[e.symbol]);
    on_simulate(e.symbol);
    // forward to algorithms
    $quotes.on_next(std::move(e));
  }

  // quotes from the gamma strategy  -  receive the gamma
  virtual void on_next(TOrderMessage e) {
    on_clock(e.rtime);
    dlog<info>(e);
    assert(!std::isnan(e.price));
    assert(!std::isnan(e.qty));
    gamma(e.side())[e.symbol] = fabs(e.qty);
    quotes(e.side())[e.symbol] = e.price;
    xlog<info>("SIM.ORDR", e.symbol, quotes[e.symbol], market[e.symbol]);
    on_simulate(e.symbol);
  }

  virtual void on_session_start(double dtime) {
    if(std::isnan(dtime)) {
        throw std::runtime_error("on_session_start(NAN)");
    }
    TSessionMessage e;
    e.ctime = e.rtime = dtime-1e-6; //FIXME: should be first message in order of sending without this hack
    $session.on_next(e);
  }
  
  bool should_fill(double overlap) {
    return overlap >= - eps();  // equal with rounding error or greater
  }
  
  virtual void on_simulate(const SymbolId& s) {
    ExecutionMessage e;
    e.set_flag(ExecutionMessage::IS_FILL);
    e.ctime = e.rtime = dt;
    e.symbol = s;
    e.qty_leaves = 0;
    auto m = market[s];
    auto q = quotes[s];
    auto pi = mpi[s];
    auto g = gamma[s];
    assert(!std::isnan(pi));
    if(!std::isnan(q.sell) && should_fill(m.buy-q.sell)) {
      // simulate the sells
      e.price = m.buy;
      e.fill_price = 0.5*(m.buy+q.sell);
      e.qty = - (g.sell + roundl((m.buy-q.sell)/pi)*g.sell);
      // move up sell quote since it got filled
      quotes.sell[s] = m.buy + pi;
      xlog<info>("SIM.SELL", s, quotes[s], m, e.fill_price, e.qty);
      check(e);
      $execs.on_next(e);
    }
    if(!std::isnan(q.buy) && should_fill(-(m.sell-q.buy))) {
      // simulate the buys
      e.price = m.sell;
      e.fill_price = 0.5*(m.sell+q.buy);
      e.qty =  (g.buy + roundl((q.buy-m.sell)/pi)*g.buy);
      // move down buy quote since it got filled
      quotes.buy[s] = m.sell -pi;
      xlog<info>("SIM.BUY ", s, quotes[s], m, e.fill_price, e.qty);
      check(e);
      $execs.on_next(e);
    }
  }

  void check(const ExecutionMessage &e) {
      if(fabs(e.qty)>=check_big_qty) {
        auto s = e.symbol;
        xlog<warn>("BIG.QTY!", e.symbol, quotes[s], market[s], e.fill_price, e.qty);
      }
  }
  
  void notify(double dt)  {
    scheduler.on_execute(dt);
  }
};

}; // ns Rcpp
#endif // GAMMASIM_H

