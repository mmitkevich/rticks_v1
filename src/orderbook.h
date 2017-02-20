#ifndef __RT_ORDERBOOK_H__
#define __RT_ORDERBOOK_H__

namespace Rcpp {

/*** OrderLevel = (price, volume) */
class PriceVolume {
private:
  double price;
  double volume;
public:
  PriceVolume(double price_=NAN, double volume_=NAN) :
  price(price_),
  volume(volume_) {
    
  }
  
  PriceVolume(const PriceVolume & rhs) :
  price(rhs.price),
  volume(rhs.volume) {
  }
  
  PriceVolume &resize(double volume_) {
    volume = volume_;
    return *this;
  }
};

class Order : public PriceVolume {
public:
  int status;
  int id;
  int symbol;
  
  enum{
    NEW         = 0x01,
    FILLED      = 0x10,
    CANCELLED   = 0x20,
    PART_FILLED = 0x40,
  };
  
public:
  
  Order(double price_ = NAN, 
        double volume_ = NAN, 
        int symbol_ = -1, 
        int status_ = NEW,
        int id_ = -1) : 
    PriceVolume(price_, volume_) {
    status = status_;
    id = id_;
    symbol = symbol_;
  }
};

#define ASSERT(x)

template<typename T, typename N>
struct list_node
{
  T value;
  N next;
  
  list_node(const T& value_, const N& next_): 
    value(value_), 
    next(next_) { 
  }
};

template<typename T, typename N>
struct bilist_node : public list_node<T, N>
{
  N prev;
  
  bilist_node(const T& value_, const N& next_, const N& prev_) : 
    list_node<T,N>(value_, next_),
    prev(prev_) { 
  }
};

class OrderLevels
{
public:
    typedef PriceVolume order_t;
    std::vector<PriceVolume> bids;
    std::vector<PriceVolume> asks;
};

// TODO: OrderBook is for simulator 
template<typename TOrder>
class OrderBook
{
  typedef typename TOrder::price_t price_t;
  typedef std::vector<list_node<TOrder, TOrder*> > arena_t;
  arena_t arena;
  typedef std::unordered_map<price_t, bilist_node<int, price_t> > index_t;
  typedef typename index_t::iterator index_iterator_t;
  index_t index;
  int ihead;
  int itail;
  price_t best_bid;
  price_t best_ask;
  
  struct iterator {
    OrderBook *book;
    int index;
    index_iterator_t index_itr;
    bool has_next() {
      return (*index_itr);
    }
  };
public:
    int ibid, iask;

public:
  OrderBook() {
    arena.resize(256);
    ihead = 0;  // _ _ X X X X _ _ _
    itail = -1; //     T     H
  }

  void place(const TOrder &order) {
      arena[ihead].value = order;
      index_iterator_t iprice = index.find(order.price);
      //arena[ihead].next = (iprice == index.end()) ? 
  }
  
  OrderBook &operator=(const OrderBook &rhs) {
    ibid = rhs.ibid;
    iask = rhs.iask;
    return *this;
  }
};

} // namespace Rcpp
#endif