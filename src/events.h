#pragma once
#ifndef _EVENTS_H_
#define _EVENTS_H_

#include "algo.h"

namespace Rcpp {

extern int Messages_Constructed;

struct Message {
  unsigned long flags;
  double datetime;
  SymbolId symbol;
  enum {
    FROM_MARKET = 1,  // or from strategy ?
    IS_BUY      = 2,
    IS_SELL     = 4,
    IS_CANCEL   = 8,
    IS_FILL     = 16
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

  int side() const {
      if(flags & IS_BUY)
          return OrderSide::BUY;
      if(flags & IS_SELL)
          return OrderSide::SELL;
      return OrderSide::UNKNOWN;
  }

  void set_side(int side) {
      if(side>0) {
          set_flag(IS_BUY);
          clear_flag(IS_SELL);
      }else if(side<0) {
          set_flag(IS_SELL);
          clear_flag(IS_BUY);
      }else {
          clear_flag(IS_SELL|IS_BUY);
      }
  }

  operator double() const {
      return datetime;
  }

  auto as_tuple() const {
      return std::make_tuple(flags, datetime, symbol);
  }
};

typedef Algo MarketAlgo;

/*std::ostream& operator<< (std::ostream &os, const Message &msg) {
    os << "(" << msg.datetime.format() << ", "<< msg.symbol << ")" << "\n";
    return os;
}*/

struct SymbolMessage : public Message {
  double mpi;

  SymbolMessage()
    : mpi(NAN){ }
};

/// L1 bid/ask spread algorithm output
struct QuoteMessage : public Message {
  double price;
  double qty;
  enum {
    HIGH_LOW    = 2<<16, // these quotes are high/low aggregates
  };
  QuoteMessage() { }
};

template<typename T>
struct ValueMessage : public Message {
  T value;
  ValueMessage() { }
  ValueMessage(T value) : value(value) { }
  ValueMessage(const ValueMessage &rhs) = default;

  typedef Observable<ValueMessage, IObserver<ValueMessage> > stream_type;
};

struct OrderMessage : public Message {
  double qty;           // order qty
  double price;         // order price

  auto as_tuple() const {
      return Message::as_tuple() & std::make_tuple(qty, price);
  }
};

/// placed/moved/cancelled/filled
struct ExecutionMessage : public OrderMessage {
  double qty_leaves;        // outstanding qty
  double fill_price;        // average price
  double price;             // order price (for multiple aggregated executions will be last fill price)

  int side() const {
    return qty>0 ? OrderSide::BUY : OrderSide::SELL;
  }
  
  bool is_partial_fill() {
      return is_fill() && !is_zero(qty_leaves);
  }

  bool is_fill() {
      return flag(Message::IS_FILL);
  }

  bool is_full_fill() {
      return is_fill() && is_zero(qty_leaves);
  }

  bool is_cancel() {
      return flag(IS_CANCEL);
  }

  ExecutionMessage()
      : fill_price(NAN),
        qty_leaves(NAN) { }
};


/// in respond to position request
struct PositionMessage: public Message {
  double pos;       // updated position
  // TODO: margin ?

  PositionMessage() : pos(NAN) { }
};


/// gamma algorithm output
struct GammaMessage : public OrderMessage {
  double gamma;

  GammaMessage() : gamma(NAN) { }

  auto as_tuple() {
    return OrderMessage::as_tuple() & std::make_tuple(gamma);
  }
};

// stream of messages
template<typename TOutput,
         typename TObserver=IObserver<TOutput>,
         typename TBase=Observable<TOutput, TObserver> >
struct Stream : public TBase {
    using TBase::notify;
};

template<typename TInput,
         typename TOutput=TInput,
         typename TObserver=IObserver<TOutput>,
         typename TBase=Processor<TInput, TOutput, TObserver> >
struct StreamProcessor : public TBase {
    using TBase::notify;
};


}; //namespace Rcpp
#endif

