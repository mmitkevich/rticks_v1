#pragma once
#ifndef _EVENTS_H_
#define _EVENTS_H_

#include "algo.h"

namespace Rcpp {

extern int Messages_Constructed;

struct Message {
  unsigned long flags;
  double ctime;     // CREATED time
  double rtime;     // RECEIVED time
  SymbolId symbol;
  enum {
    MSGTYPE     = 0xFF,
    FROM_MARKET = 2<<9,  // or from strategy ?
    IS_BUY      = 2<<10,
    IS_SELL     = 2<<11,
    IS_CANCEL   = 2<<12,
    IS_FILL     = 2<<13,
    IS_PLACE    = 2<<14,
  };

  Message(double ctime = NAN, double rtime = NAN, SymbolId symbol = SymbolId(), unsigned long flags = 0) :
    flags(flags),
    ctime(ctime),
    rtime(rtime),
    symbol(symbol) {
#ifdef DEBUG_MESSAGE
    std::cerr << "Message#" << Messages_Constructed;
    Messages_Constructed++;
#endif
  }

  int msgtype() const {
    return flags & MSGTYPE;
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

//  operator double() const {
//      return datetime;
//  }

  // small time should be GREATER priority than big time
  bool operator<(const Message &rhs) const {
    return rtime>rhs.rtime;
  }

  auto as_tuple() const {
      return std::make_tuple(flags, rtime, symbol);
  }

};


struct MarketAlgo : public Algo {
    NumericVector pos;     // latest position
    NumericVector mpi;
    BuySellVector market;  // latest market prices
    BuySellVector quotes;  // latest our bid & ask
    CharacterVector symbols;

    MarketAlgo(    DataFrame params,
                   List config,
                   std::string name = "")
        : Algo(params, config, name),
          symbols(required<CharacterVector>(params, "symbol")),
          market(params.nrows()),
          quotes(params.nrows()),
          pos(optional<NumericVector>(params, "pos", 0)),
          mpi(required<NumericVector>(params, "mpi"))
    {
    }

    template<int level>
    void xlog(std::string what, SymbolId sym, const BuySell &q, const BuySell &m, double fill_price=NAN, double fill_qty=NAN) {
        if(level>=log_level) {
            //if(logger)
            //  logger->log(spdlog::level::info, "{} | {} | {}={} | M={}, {} | Q={}, {} | {} | {}", // FIXME ::(spdlog::level::level_enum)level
            //             Datetime(dt), what, sym.id, sym.index, m.buy, m.sell, q.buy, q.sell, fill_price, fill_qty);
            //else
              std::cout << Datetime(dt) << " | " << what << " | " << sym.id << " | " << sym.index <<
                           " | " << m.buy << " " << m.sell <<
                           " | " << q.buy << " " << q.sell <<
                           fill_price << fill_qty << std::endl << std::flush;
        }
    }

};

std::ostream& operator<< (std::ostream &os, const Message &e) {
    os << e.symbol.id << "=" << e.symbol.index;
    return os;
}

struct SymbolMessage : public Message {
  double mpi;

  SymbolMessage()
    : mpi(NAN){ }
};

/// L1 bid/ask spread algorithm output
struct SessionMessage : public Message {
  SessionMessage() { }
};

/// L1 bid/ask spread algorithm output
struct QuoteMessage : public Message {
  double price;
  double qty;
  enum {
    HIGH_LOW    = 2<<16, // these quotes are high/low aggregates
  };

  int side() const {
    return qty>0 ? OrderSide::BUY : OrderSide::SELL;
  }

  QuoteMessage() { }
};

std::ostream& operator<< (std::ostream &os, const QuoteMessage &e) {
    os << (const Message&) e << " | Q="
       << (e.flag(QuoteMessage::HIGH_LOW)?"X":"") << (e.side()>0?"B":"S")
       << " | p="<< e.price << " | q=" << e.qty;
    return os;
}

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

  int side() const {
    return qty>0 ? OrderSide::BUY : OrderSide::SELL;
  }

  bool is_cancel() const {
      return flag(IS_CANCEL);
  }

  bool is_place() const {
      return flag(IS_PLACE);
  }


  auto as_tuple() const {
      return Message::as_tuple() & std::make_tuple(qty, price);
  }
};

std::ostream& operator<< (std::ostream &os, const OrderMessage &e) {
    os << (const Message&) e<< " | O=" << (e.is_cancel() ? "C":"") << (e.is_place() ? "P":"")
       << (e.side()>0?"B":"S")
       << " | p="<< e.price << " | q=" << e.qty;
    return os;
}


/// placed/moved/cancelled/filled
struct ExecutionMessage : public OrderMessage {
  double qty_leaves;        // outstanding qty
  double fill_price;        // average price
  
  bool is_partial_fill() const {
      return is_fill() && !is_zero(qty_leaves);
  }

  bool is_fill() const {
      return flag(Message::IS_FILL);
  }

  bool is_full_fill() const {
      return is_fill() && is_zero(qty_leaves);
  }

  ExecutionMessage()
      : fill_price(NAN),
        qty_leaves(NAN)
        { }
};

std::ostream& operator<< (std::ostream &os, const ExecutionMessage &e) {
    os << (const OrderMessage&) e << " | E="
       << (e.is_partial_fill()?"P":"") << (e.is_fill()?"F":"") << (e.is_cancel()?"C":"")
       <<" | fp="<< e.fill_price << " | ql=" << e.qty_leaves;

    return os;
}

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

  auto as_tuple() const {
    return OrderMessage::as_tuple() & std::make_tuple(gamma);
  }
};

std::ostream& operator<< (std::ostream &os, const GammaMessage &e) {
    os << (const OrderMessage&) e << " | G | g="<< e.gamma;
    return os;
}

//std::ostream& operator<< (std::ostream &os, const Datetime &dt) {
//  os << dt.format();
//  return os;
//}

void format_arg(fmt::BasicFormatter<char> &f, const char *&format_str, const Rcpp::Datetime &dt) {
  f.writer().write(dt.format());
}

}; //namespace Rcpp
#endif

