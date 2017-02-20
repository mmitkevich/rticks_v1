#ifndef __RT_BACKTEST_H__
#define __RT_BACKTEST_H__

#include <vector>
#include <memory>
#include <deque>
#include <unordered_map>
#include <Rcpp.h>

#include "utils.h"
#include "events.h"
#include "orderbook.h"

namespace Rcpp {

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

class IGateway
{
  void subscribe() {
      
  }
};


/** Each strategy has output performance (realized_pnl, equity, pos) (by each instrument it trades) */

template<typename TContext = IGateway, 
         typename TOrderLevels = OrderLevels, 
         typename TOrder = Order,
         typename TOrderBook = OrderLevels > // OrderBook<TOrder>
class Algorithm 
{
protected:
  TContext *ctx;
  TOrderLevels levels;
  TOrderBook book;
  double volume_default;
public:
  typedef TOrderBook book_t;
  typedef TOrderLevels levels_t;
  typedef TOrder order_t;
public:
  Algorithm() : 
    ctx(NULL) {
  }
  
  void on_init(TContext *ctx_) {
    ctx = ctx_;
    volume_default = 1.0; 
  }
  
  const PriceVolume& bid(int i = 0) {
    return book.bids[0];
  }

  const PriceVolume& ask(int i = 0) {
    return book.asks[0];
  }
  
  void buy(double volume = NAN, double price = NAN) {
    TOrderLevels::order_t &bid = levels.bid(0);
    bid.volume = std::isnan(volume) ? volume_default : volume;
    bid.price = std::isnan(price) ? ask().price : price;
  }
  
  void sell(double volume = NAN, double price = NAN) {
    TOrder  &ask = levels.ask(0);
    ask.volume = std::isnan(volume) ? volume_default : volume;
    ask.price = std::isnan(price) ? bid().price : price;
  }
  
  void on_order_book(Event<TOrderBook> &book_) {
    book = book_;
    on_update();
  }
  
  void on_order_status(Event<TOrder> &) {
    
  }
  
  void on_update() {
    
  }
};

class Simulator : public IGateway
{
public:
  template<typename TAlgorithm>
  List backtest(DataFrame data, DataFrame params, List config) {

    typedef typename TAlgorithm::book_t TOrderBook;
    typedef typename TAlgorithm::book_t::order_t TOrder;

    int nparams = params.nrows();
    
    std::vector<TAlgorithm> strategy(params.size());
    for(int i=0; i<params.size(); i++) {
      strategy[i].on_init(this);
    }
  
    int itime = 0;
    int ntime  = data.nrows();
    NumericVector datetimes = as<NumericVector>(data["datetime"]);
    CharacterVector events = as<CharacterVector>(data["event"]);
    NumericVector values = as<NumericVector>(data["value"]);
    TOrderBook book;
    double quote_volume_default = 1e6;
    while(itime < ntime) {
      std::string evt((const char*)events[itime]);
      double val = values[itime];
      if(evt == "bid" || evt == "high") 
        book.bids[0] = TOrder(quote_volume_default, val);
      else if(evt == "ask" || evt == "low")
        book.asks[0] = TOrder(quote_volume_default, val);
      for(int ipar = 0; ipar < params.size(); ipar++) {
        //strategy[ipar].on_order_book(Event<TOrderBook>(book, itime));
      }
      itime++;
    }
  }
};

} // namespace Rcpp
#endif