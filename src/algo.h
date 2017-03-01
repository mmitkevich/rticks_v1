#pragma once

#ifndef ALGO_H
#define ALGO_H

#include "utils.h"
#include "reactive.h"

namespace Rcpp {

namespace OrderSide {
  enum {
    BUY = 1,
    SELL = -1
  };
};

struct SymbolId {
  const char* id;
  int index;

  SymbolId(const char* id=NULL, int index=-1)
    : id(id), index(index) { }

  operator int() const {
    return index;
  }
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

template <typename OutputStream>
OutputStream & operator<< (OutputStream &os, const SymbolId &s) {
    if(s.id!=NULL)
        os << s.id;
    else
        os << "?";
    os << "#";
    if(s.index!=-1)
        os << s.index;
    else
        os << "?";
    return os;
}


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

}; //ns Rcpp

#endif // ALGO_H

