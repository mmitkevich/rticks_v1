#pragma once

#ifndef ALGO_H
#define ALGO_H

#include "reactive.h"
#include <cmath>

namespace Rcpp {

namespace OrderSide {
  enum {
    BUY = 1,
    SELL = -1,
    UNKNOWN = 0
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
struct Message;

struct IAlgo {
    enum {
        EMPTY  =  0x0001
    };

    virtual void notify() = 0;  // flush notifications to subscribers
    virtual double datetime() = 0;   // current timestamp
    virtual ~IAlgo(){ }
    virtual bool operator<(IAlgo &rhs) {
        return datetime() < rhs.datetime();
    }
};


struct Algo : public IAlgo {
  List config;
  DataFrame params;
  double dt;
  std::string name;
  Algo(
    DataFrame params, // (symbol, mpi, par1, par2,....)
    List config,
    std::string name = "")
      : config(config),
        params(params),
        dt(NAN),
        name(name)
        { }

  int size() {
      return params.size();
  }

  virtual double datetime() {
     return dt;
  }

  virtual void notify() {

  }

  template<typename TMessage>
  void on_recv(const TMessage& e) {
    std::cout << name << " " << e;
    std::cout << std::flush;
  }
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

  int count_buy() const {
    return std::isnan(buy) ? 1:0;
  }

  int count_sell() const {
    return std::isnan(sell) ? 1:0;
  }

  int count(int side) const {
      return side>0 ? count_buy() : count_sell();
  }

  double slippage(int side, double qty) const {
    return NAN;
  }

  double average(int side, double qty) const {
    return (*this)(side);
  }

  double& operator()(int side) {
    return side>0 ? buy:sell;
  }

  double operator()(int side) const {
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
    : buy(std::move(buy)),
      sell(std::move(sell)) {
  }

  BuySellVector(int n)
    : buy(n,NAN),
      sell(n,NAN) {
  }

  BuySellVector() {
    buy = NAN;
    sell = NAN;
  }

  BuySellVector(const BuySellVector& rhs) = default;


  const BuySell operator[](int index) const {
    return BuySell(buy[index], sell[index]);
  }

  template<typename TMessage>
  void update(int index, const TMessage &e) {
    if(e.side()>0)
        buy[index] = e.price;
    else
        sell[index] = e.price;
  }

  void reset(int index, int side, double price) {
      if(side>0)
          buy[index] = price;
      else if(side<0)
          sell[index] = price;
  }

  NumericVector& operator()(int side) {
    if(side>0)
      return buy;
    return sell;
  }
};

}; //ns Rcpp

#endif // ALGO_H

