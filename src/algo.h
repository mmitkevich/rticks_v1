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

  SymbolId(const SymbolId &rhs)
      :id(rhs.id), index(rhs.index) {

  }

  operator int() const {
    return index;
  }
};

struct Algo  {
  double datetime;
  List config;
  DataFrame params;

  Algo(
    DataFrame params, // (symbol, mpi, par1, par2,....)
    List config)
      : datetime(NAN),
        config(config),
        params(params) { }
};

/*std::ostream & operator<< (std::ostream &os, const SymbolId &s) {
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
}*/


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

template<typename TValue,
         typename TVector>
struct vector_iterator {
    TVector *parent;
    int index;
    typedef TValue value_type;

    vector_iterator(TVector *p, int index)
        : parent(p), index(index) { }

    vector_iterator(const vector_iterator &rhs)
      : parent(rhs.parent),
        index(rhs.index)
    { }

    bool operator==(const vector_iterator &rhs){
        return index==rhs.index;
    }

    TValue operator*() {
        return parent[index];
    }
};

template<typename TValue, typename TVector>
struct vector_base
{
    typedef vector_iterator<TValue, TVector> iterator;

    iterator begin() {
      return iterator(self());
    }

    iterator end() {
        return iterator(self(), self()->size());
    }
private:
    TVector* self() {
        return static_cast<TVector*>(this);
    }
};

struct BuySellVector : public vector_base<BuySell, BuySellVector> {
  NumericVector buy;
  NumericVector sell;

  int size() const {
      return buy.size();
  }

  BuySellVector(NumericVector buy, NumericVector sell)
    : buy(buy),
      sell(sell) {
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

  BuySellVector(int n)
    : buy(n,NAN),
      sell(n,NAN) {
  }

  BuySellVector() { }

  BuySell operator[](int index) {
    return BuySell(buy[index], sell[index]);
  }

  void update(int index, BuySell val) {
    buy[index] = val.buy;
    sell[index] = val.sell;
  }

  BuySellVector& operator=(BuySell val) {
    buy = val.buy;   // TODO: std::forward vs std::move
    sell = val.sell;
  }

  NumericVector& operator()(int side) {
    if(side>0)
      return buy;
    return sell;
  }
};

}; //ns Rcpp

#endif // ALGO_H

