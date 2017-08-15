#pragma once

#include "events.h"
#include "metrics.h"
#include "gammasim.h"

namespace Rcpp {

const double MAX_LATENCY = 1; //second


struct SignalPlayer {
  std::vector<int> index;
  List signals;  
  typedef std::function<void(const DataFrame &df, int)> observer_type;
  observer_type obs;
  
  SignalPlayer(List signals, observer_type obs):
    signals(signals),
    obs(obs)
  {
    index.resize(signals.size(), 0);
  }
  
  void notify(double dt) {
    for(;;) {
      int min_index = -1;
      double min_dt = +INFINITY;
      for(int i=0; i < index.size(); i++) {
        DataFrame df = as<List>(signals[i]);
        if(index[i] < df.nrows()) {
          auto dt1 = as<NumericVector>(df[0])[index[i]];
          if(dt1 <= dt+eps()) {
            min_dt = std::min(min_dt, dt1);
            min_index = i;
          }
        }
      }
      if(min_index >= 0) {
        DataFrame df = as<List>(signals[min_index]);
        index[min_index]++;
        obs(df, min_index);        
      }else
        break;
    }
  }
};

template<typename TAlgo,
         typename TMarket=GammaSimulator<typename TAlgo::order_message_type>>
struct Backtester : public Algo
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

  TMarket market;
  TAlgo algo;
  double close_time_fix;
  
  Backtester(DataFrame params,     // symbol, mpi, spread, buy_gamma, sell_gamma
            List config)                // metrics_interval
    : Algo(params, config, "player"),
      market(params, config),
      algo(params, config),
      close_time_fix(0),  // event occured 1 second before close time
      index(0),
      stop(0),
      mpi(required<NumericVector>(params, "mpi")),
      symbols(required<CharacterVector>(params, "symbol"))
  {
      algo.on_init(market);
  }
  
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

  // data = datetime, symbol, bid, ask, high, low
  // signals = List(DF(datetime,values))
  void process(DataFrame data, List signals)    
  {
    datetimes = required<NumericVector>(data, "datetime");
    if(datetimes.size()==0)
        return;

    bids = required<NumericVector>(data, "bid");
    asks = required<NumericVector>(data, "ask");
    highs = optional<NumericVector>(data, "high");
    lows = optional<NumericVector>(data, "low");
    virtual_symbol = required<CharacterVector>(data, "virtual_id");
    
//    logger->warn("rticks::Backtester::process {}", Datetime(datetimes[0]));
    
    index = 0;
    stop = data.nrows();
    dt = datetimes[0];

    double close_dt = NAN;

    size_t sent_events = 0;
    
    for(int i=0;i<symbols.size();i++) {
      SessionMessage ses;
      ses.ctime = ses.rtime = dt;
      ses.set_flag(Message::IS_SESSION_OPENED);
      ses.symbol = to_symbol_id(symbols[i]);
      market.on_next(ses);
    }
    
    SignalPlayer signal_player(signals, [&](const DataFrame &df, int index){
      ValueMessage<double> msg;
      msg.rtime = msg.ctime = as<NumericVector>(df[0])[index];
      msg.value = as<NumericVector>(df[1])[index];
      CharacterVector nms = df.names();
      msg.param = SymbolId(nms[index], index);
      msg.symbol = to_symbol_id(as<CharacterVector>(df[2])[index]);
      algo.on_next(msg);
    });
    
    while(index < stop) {
      dt = datetimes[index] - close_time_fix;

      market.notify(dt); // flush the time
      
      if(logger && debug>=log_level) {
        logger->info("CANDLE {} | {}, {}", Datetime(dt), bids[index], asks[index]);
      }
      
      if(!std::isnan(close_dt) && dt<=close_dt+eps()) {
          auto s = fmt::format("datetime already seen. dt={}, prev={}, index={}, symbol={}", Datetime(dt), Datetime(close_dt), index, virtual_symbol[index]);
          if(logger)
              logger->warn(s);
          index++;
          Rcpp::stop(s);
          continue;
      }
      if(bids[index]>asks[index]+eps()) {
          auto s = fmt::format("bid>ask - skipping, bid=[}, ask={}, dt={}, index={}, symbol={}", bids[index], asks[index], Datetime(dt), index, virtual_symbol[index]);
          if(logger)
              logger->warn(s);
          index++;
          Rcpp::stop(s);
          continue;
      }

      signal_player.notify(dt);
      
      auto s = to_symbol_id(virtual_symbol[index]); //TODO: fix symbol search via hashmap
      double time2 = dt + 1e-6;
      if(s.index>=0) {
        // buffer events
        QuoteMessage bid;
        bid.set_flag(Message::FROM_MARKET);
        bid.set_side(OrderSide::BUY);
        bid.symbol = s;
        bid.ctime = bid.rtime = dt;
        bid.price = bids[index];
        bid.qty = +INFINITY;
        
        QuoteMessage ask;
        ask.set_flag(Message::FROM_MARKET);
        ask.set_side(OrderSide::SELL);
        ask.symbol = s;
        ask.ctime = ask.rtime = dt;
        ask.price = asks[index];
        ask.qty = -INFINITY;
        
        if(index>0 && bid.price>=asks[index-1]-eps()) {
          // this is more realistic sequence
          market.on_next(ask);
          bid.ctime = bid.rtime = time2; // make bid later
          market.on_next(bid);
          sent_events++;
        }else {
          market.on_next(bid);  // send bid
          ask.ctime = ask.rtime = time2; // make ask later
          market.on_next(ask);  // send ask
          sent_events++;
        }
      }else {
        if(logger)
          logger->warn("Unknown symbol {}",s.id);
      }
      
      close_dt = dt;
      index++;
    } // while

    dt+=1e-6;
    for(int i=0;i<symbols.size();i++) {
      SessionMessage ses;
      ses.ctime = ses.rtime = dt;
      ses.clear_flag(Message::IS_SESSION_OPENED);
      ses.symbol = to_symbol_id(symbols[i]);
      market.on_next(ses);
    }
    dt+=MAX_LATENCY;
    market.notify(dt); // flush the time, allow 1 second latency to deliver everything
  }
};



}; //namespace Rcpp
