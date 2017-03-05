#pragma once
#ifndef GAMMASIM_H
#define GAMMASIM_H

namespace Rcpp {

template<typename TOrderMessage=OrderMessage,
         typename TQuoteMessage=QuoteMessage,
         typename TExecutionMessage=ExecutionMessage,
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
  LatencyQueue<TQuoteMessage> $quotes;
  LatencyQueue<TExecutionMessage> $execs;
  LatencyQueue<TOrderMessage> $orders;
  TScheduler scheduler;
  TMetrics metrics;

  GammaSimulator(DataFrame params, List config, std::string name="gammasim")
    : MarketAlgo(params, config, name),
      scheduler(params, config),
      metrics(params, config),
      $quotes(params, config, &scheduler, "$quotes "),
      $execs(params, config, &scheduler,  "$execs  "),
      $orders(params, config, &scheduler, "$orders ")
  {
      //quotes.buy = quotes.sell = NAN; // FIXME: this is wrong. quotes.buy and quotes.sell will be SHARED NumVector.
      // TODO: migrate to std::vector instead of NumericVector???
      $orders >>= *this;
      // wire up metrics
      $execs >>= metrics;
      //log_level = 1;
  }

  virtual void on_next(TQuoteMessage e) {
    on_clock(e.rtime);
    dlog<3>(e);
    assert(e.flag(Message::FROM_MARKET));
    market.update(e.symbol, e);
    xlog<2>("SIM.QUOT", e.symbol, quotes[e.symbol], market[e.symbol]);
    on_simulate(e.symbol);
    // forward to algorithms
    $quotes.on_next(std::move(e));
  }

  // quotes from the gamma strategy  -  receive the gamma
  virtual void on_next(TOrderMessage e) {
    on_clock(e.rtime);
    dlog<3>(e);
    assert(!std::isnan(e.price));
    assert(!std::isnan(e.qty));
    gamma(e.side())[e.symbol] = fabs(e.qty);
    quotes(e.side())[e.symbol] = e.price;
    xlog<1>("SIM.ORDR", e.symbol, quotes[e.symbol], market[e.symbol]);
    on_simulate(e.symbol);
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
    if(!std::isnan(q.sell) && m.buy >= q.sell) {
      // simulate the sells
      e.price = m.buy;
      e.fill_price = 0.5*(m.buy+q.sell);
      e.qty = - (g.sell + truncl((m.buy-q.sell)/pi)*g.sell);
      // move up sell quote since it got filled
      quotes.sell[s] += pi;
      xlog<1>("SIM.SELL", s, quotes[s], m, e.fill_price, e.qty);
      $execs.on_next(e);
    }
    if(!std::isnan(q.buy) && m.sell <= q.buy) {
      // simulate the buys
      e.price = m.sell;
      e.fill_price = 0.5*(m.sell+q.buy);
      e.qty =  (g.buy + truncl((q.buy-m.sell)/pi)*g.buy);
      // move down buy quote since it got filled
      quotes.buy[s] -= pi;
      xlog<1>("SIM.BUY ", s, quotes[s], m, e.fill_price, e.qty);
      $execs.on_next(e);
    }
  }

  void notify(double dt)  {
    scheduler.on_execute(dt);
  }
};

}; // ns Rcpp
#endif // GAMMASIM_H

