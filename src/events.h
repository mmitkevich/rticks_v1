#pragma once
#ifndef _EVENTS_H_
#define _EVENTS_H_

//#include "Rcpp/date_datetime/Datetime.h"

#include "algo.h"

namespace Rcpp {

extern int Messages_Constructed;

struct Message {
  unsigned long flags;
  Datetime datetime;
  SymbolId symbol;
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
  
  int flag(int mask=-1) const {
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

/*std::ostream& operator<< (std::ostream &os, const Message &msg) {
    os << "(" << msg.datetime.format() << ", "<< msg.symbol << ")" << "\n";
    return os;
}*/

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

template<typename T>
struct ValueMessage : public Message {
  T value;
  ValueMessage() { }
  ValueMessage(T value) : value(value) { }
  ValueMessage(const ValueMessage &rhs) = default;
};

struct PositionUpdated: public Message {
  double qty;
  double pos;

  int side() const {
    return qty>0 ? OrderSide::BUY : OrderSide::SELL;
  }

  PositionUpdated() : pos(NAN), qty(NAN) { }
};

struct OrderFilled : public PositionUpdated {
  double qty_left;
  double price;

  int side() const {
    return qty>0 ? OrderSide::BUY : OrderSide::SELL;
  }
  
  OrderFilled()
      : price(NAN), qty_left(NAN) { }
};


struct GammaQuotesUpdated : public QuotesUpdated
{
  BuySell gamma;
};



}; //namespace Rcpp
#endif
