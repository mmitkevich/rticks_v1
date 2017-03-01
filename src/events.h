#pragma once

#include "algo.h"

namespace Rcpp {

extern int Messages_Constructed;

struct Message {
  double datetime;
  SymbolId symbol;
  unsigned long flags;
  enum {
    FROM_MARKET = 2<<0,  // or from strategy ?
  };


  Message(double datetime = NAN, SymbolId symbol = SymbolId(), unsigned long flags = 0) :
    flags(flags),
    datetime(datetime),
    symbol(symbol) {
#ifdef DEBUG_MESSAGE
    std::cerr << "Message#" << Messages_Constructed;
    Messages_Constructed++;
#endif
  }
  
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

  operator double() const {
      return datetime;
  }
};

template <typename OutputStream>
OutputStream & operator<< (OutputStream &os, const Message &msg) {
    os << "(" << msg.datetime << ", "<< msg.symbol << ")" << "\n";
    return os;
}

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
