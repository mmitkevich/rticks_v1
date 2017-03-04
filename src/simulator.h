#pragma once

#include "events.h"
#include "metrics.h"

namespace Rcpp {


struct Scheduler: public Algo {
    std::priority_queue<Algo*> queue;

    Scheduler(DataFrame params, List config)
        :Algo(params, config) { }

    void on_recv(Algo* algo, double dt) {
        queue.push(algo);
    }

    void schedule(double time) {
        while(!queue.empty()) {
            auto algo = queue.top();
            if(algo->datetime() <= time) {
                queue.pop();
                algo->notify();
            }
        }
    }
};


template<typename TMessage=Message>
struct DelayedStream : public Algo,
              public StreamProcessor<TMessage>
{
    using StreamProcessor<TMessage>::notify;

    std::priority_queue<TMessage> queue;
    Scheduler *scheduler;

    DelayedStream(DataFrame params, List config, Scheduler *scheduler)
        : Algo(params, config),
          scheduler(scheduler) {
    }

    virtual void on_next(TMessage e) {
        queue.push(e);
    }

    virtual void notify() {
        notify(queue.top());
        queue.pop();
    }

    void enqueue(TMessage e) {
        queue.push(e);
    }

    virtual bool empty() {
        return queue.empty();
    }
};

template<typename TOrderMessage=OrderMessage,
         typename TQuoteMessage=QuoteMessage,
         typename TExecutionMessage=ExecutionMessage>
struct Player : public Scheduler
{
    // from params
  NumericVector mpi;
  CharacterVector symbols;
  
  // from data
  NumericVector datetimes;
  NumericVector bids;
  NumericVector asks;
  NumericVector highs;
  NumericVector lows;
  CharacterVector virtual_symbol;

  int index;
  int stop; 

  /// outputs
  DelayedStream<TQuoteMessage> $quotes;
  DelayedStream<TExecutionMessage> $execs;
  DelayedStream<TOrderMessage> $orders;

  Player(DataFrame params,     // symbol, mpi, spread, buy_gamma, sell_gamma
            List config)                // metrics_interval
    : Scheduler(params, config),
      $quotes(params, config, this),
      $execs(params, config, this),
      $orders(params, config, this),
      index(0),
      stop(0),
      mpi(required<NumericVector>(params, "mpi")),
      symbols(required<CharacterVector>(params, "symbol"))
      {  }
  
  SymbolId to_symbol_id(const char* sym) {
      for(int i=0;i<symbols.size();i++){
          if(symbols[i]==sym) {
              return SymbolId(sym, i);
          }
      }
      std::stringstream s;
      s << "symbol '" << sym << "' not found in params";
      Rcpp::stop(s.str());
  }

  void process(DataFrame data)    // datetime, symbol, bid, ask, high, low
  {
    datetimes = required<NumericVector>(data, "datetime");
    bids = required<NumericVector>(data, "bid");
    asks = required<NumericVector>(data, "ask");
    highs = optional<NumericVector>(data, "high");
    lows = optional<NumericVector>(data, "low");
    virtual_symbol = required<CharacterVector>(data, "virtual_id");

    index = 0;
    stop = data.nrows();
    double close_dt = datetimes.size() >0 ? datetimes[1] : datetimes[0];
    double open_dt = datetimes[0];
    while(index < stop) {
      double dt = datetimes[index];
      if(dt<close_dt) {
          std::stringstream s;
          s << "datetimes not sorted " << dt << ", " << close_dt;
          Rcpp::stop(s.str());
      }
      // buffer events
      QuoteMessage e;
      e.set_flag(Message::FROM_MARKET);
      e.set_side(OrderSide::BUY);
      e.symbol = to_symbol_id(virtual_symbol[index]);
      e.datetime = open_dt;
      e.price = bids[index];

      $quotes(e);  // send bid

      e.set_side(OrderSide::SELL);
      e.price = asks[index];
      $quotes(e);  // send ask

      open_dt = close_dt;
      close_dt = datetimes[index];
      index++;
    }
  }
};



}; //namespace Rcpp
