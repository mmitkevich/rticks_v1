#pragma once
#include "utils.h"
namespace Rcpp {

namespace OrderSide {
  enum {
    BUY = 1,
    SELL = -1
  };
};

struct Algo  {
  double datetime;
  
  DataFrame params;
  List config;
  
  
  Algo(
    DataFrame params, // (symbol, mpi, par1, par2,....)
    List config) : 
    params(params), 
    config(config), 
    datetime(NAN)
  { }
};

template<typename TOutput, typename TObserver>
struct Observable {
  typedef TObserver observer_type;
  observer_type* observer;

  Observable() : observer(NULL) {
  }  

  // preprocess output
  virtual void notify(const TOutput &e) {
    observer->on_next(e);
  }
  
  void subscribe(TObserver *obs) {
    observer = obs;
  }
};

template<typename TInput>
struct Observer {
  typedef TInput value_type;
  virtual void on_next(const TInput &e) = 0;
};

template<typename TOutput, typename TObserver>
struct MultiObservable {
  typedef TOutput result_type;
  typedef TObserver observer_type;
  typedef std::vector<observer_type*> observers_type;
  observers_type observers;
  
  MultiObservable() {
  }  
  
  // preprocess output
  virtual void notify(TOutput &e) {
    for(int i=0; i<observers.size(); i++)
      observers[i]->on_next(e);
  }
  
  void subscribe(TObserver *obs) {
    observers.push_back(obs);
  }
};

struct SymbolId {
  const char* id;
  int index;
  
  SymbolId(const char* id, int index=-1) 
    : id(id), index(index) { }
  
  operator int() const {
    return index;
  }
};

struct BuySell {
  double buy;
  double sell;
  
  BuySell() 
    : buy(NAN), sell(NAN) { }
  
  BuySell(double buy, double sell)
    : buy(buy), sell(sell) { }
  
  BuySell(const BuySell &rhs) 
    : buy(rhs.buy), sell(rhs.sell) { }
  
  BuySell &operator=(const BuySell &rhs) {
    buy = rhs.buy;
    sell = rhs.sell;
    return *this;
  }
  
  double& operator()(int side) {
    return side>0 ? buy:sell;
  }
  
};

struct BuySellVector {
  NumericVector buy;
  NumericVector sell;
  
  BuySellVector(NumericVector buy, NumericVector sell)
    : buy(buy),
      sell(sell) { 
  }
  
  BuySellVector(int n)
    : buy(n),
      sell(n) {
  }
  
  BuySellVector() { }
  
  BuySell get(int index) {
    return BuySell(buy[index], sell[index]);
  }
  
  void update(int index, const BuySell& val) {
    buy[index] = val.buy;
    sell[index] = val.sell;
  }
  
  int size() {
    return buy.size();
  }
  
  BuySellVector(const BuySellVector& rhs) 
    : buy(rhs.buy),
      sell(rhs.sell) {
  }
  
  BuySellVector& operator=(const BuySellVector& rhs) {
    buy = rhs.buy;
    sell = rhs.sell;
    return *this;
  }
  
  NumericVector& operator()(int side) {
    if(side>0)
      return buy;
    return sell;
  }
};

class RAlgo;

struct Message {
  double datetime;
  SymbolId symbol;
  unsigned long flags;
  enum {
    FROM_MARKET = 2<<0,  // or from strategy ?
  };
  
  Message() : 
    flags(0),
    datetime(NAN),
    symbol(NULL) { }  
  
  int get_flag(int mask=-1) const {
    return flags & mask;
  }
  Message& set_flag(int mask) {
    flags |= mask;
    return *this;
  }
  Message& clear_flag(int mask) {
    flags &= ~mask;
    return *this;
  }
};

struct SymbolUpdated : public Message {
  double mpi;

  SymbolUpdated() 
    : mpi(NAN){ }
};

struct QuotesUpdated : public Message {
  BuySell quotes;
  BuySell qty; 
  enum {
    HIGH_LOW    = 2<<16, // these quotes are high/low aggregates
  };
  QuotesUpdated() { }
};

struct OrderFilled : public Message {
  double price;
  double qty;
  double left;
  
  int side() const {
    return qty>0 ? OrderSide::BUY : OrderSide::SELL;
  }
  
  OrderFilled() : price(NAN), qty(NAN), left(NAN) { }
};

struct GammaQuotesUpdated : public QuotesUpdated
{
  BuySell gamma;
};



}; //namespace Rcpp