#ifndef __RT_BACKTEST_H__
#define __RT_BACKTEST_H__

#include <Rcpp.h>
#include <vector>

struct Perfs {
  Rcpp::List result;
  Rcpp::CharacterVector symbols;
  int itime;
  int ntime;
  Rcpp::NumericVector time;
  Rcpp::DataFrame max;  // by symbol
  Rcpp::DataFrame dd;
  Rcpp::DataFrame mdd;
  Rcpp::DataFrame pos;
  Rcpp::DataFrame cash;
  Rcpp::DataFrame buy_count;
  Rcpp::DataFrame sell_count;
  Rcpp::DataFrame roundtrips;
  Rcpp::DataFrame fill_price;
  
public:
  
  Perfs(){
    itime = 0;
    ntime = 0;
  }
  
  Rcpp::DataFrame datum(const char*name, int size, double def = NAN) {
    Rcpp::List lst;
    
    for(int i=0; i<nsym(); i++) {
      lst[(const char*)symbols[i]] = Rcpp::NumericVector(size);
    }
    result[name] = lst;
    return Rcpp::DataFrame(lst);
  }
  
  int nsym() {
    return symbols.size();  
  }
  
  void init(int nt, Rcpp::CharacterVector sym) {
    ntime = nt;
    symbols = sym;
    max = datum("max", ntime);
    dd = datum("dd", ntime);
    mdd = datum("mdd", ntime);
    pos = datum("pos", ntime);
    cash = datum("cash", ntime);
    fill_price = datum("fill_price", ntime);
  }
  
  void on_bar() {
    itime++;
  }
  
  void on_trade(long sym, double q, double price) {
    add(pos, q, sym);
    add(cash, - q * price, sym);
    add(q>0 ? buy_count:sell_count, 1, sym);
    if(fabs(get(pos, sym))<1e-10)
      add(roundtrips, 1, sym);
    
    set(fill_price, price, sym); 
    //    std::cout << t << " | " << q << " * " << price << " | " << pos << " | " << cash << "\n";
    
    
  }
  
  Rcpp::NumericVector get_sym(Rcpp::DataFrame &df, int sym) {
    return Rcpp::as<Rcpp::NumericVector>(df[sym]);
  }
  
  double get(Rcpp::DataFrame &df, int sym) {
    return get_sym(df, sym)[itime];
  }
  
  void set(Rcpp::DataFrame &df, double v, int sym) {
    get_sym(df, sym)[itime] = v;
  }
  
  void add(Rcpp::DataFrame &df, double diff, int sym) {
    get_sym(df, sym)[itime] += diff;
  }
};

template<typename T> T config(Rcpp::List config, const char* name, T def) {
  return  config.containsElementNamed(name) ? (T) Rcpp::as<Rcpp::NumericVector>(config[name])[0] : def;
}

struct OrderLevel {
  double price;
  double volume;
  
  OrderLevel() : 
    price(NAN), 
    volume(0) {
    
  }
  
  OrderLevel(const OrderLevel & rhs) : 
    price(rhs.price), 
    volume(rhs.volume) {
    
  }
};

class OrderBook
{
  std::vector<OrderLevel> levels;
  int bid_index, ask_index;

private:
  int index(int i) {
    if(i>0)
      return bid_index-(i-1);
    else if(i<0)
      return ask_index+(-1-i);
    //assert(false)
    return(-1);
  }
  
public:
  OrderBook(int size) : levels(size) {
    
  }
  
  void swap(OrderBook &rhs) {
    std::swap(bid_index, rhs.bid_index);
    std::swap(ask_index, rhs.ask_index);
    std::swap(levels, rhs.levels);
  }
  
  int nasks() {
    return levels.size() - ask_index;
  }
  
  int nbids() {
    return bid_index + 1;
  }
  
  OrderLevel& level(int i = 0) {
    return levels[index(i)];
  }
};

enum OrderStatus {
  osNEW     = 0x01,
  osFILLED  = 0x10
};

struct Order {
  double price;
  double volume;
  int status;
  int id;
  int symbol;
  
  Order(double p, double v, int sym=-1):
    price(p),
    volume(v) {
    status = osNEW;
    id = -1;
    symbol = sym;
  }
};

/** Each strategy has output performance (realized_pnl, equity, pos) (by each instrument it trades) */

template<typename TPerfs, typename TSimulator>
struct Strategy 
{
private:
  int ntime;  
  /** arbitrary configuration options */
  Rcpp::List config;
  /** list with data chunk,
   * (start, stop: posix.ct; bid,ask: data.frame(datetime, MSFT, AAPL))
   */
  Rcpp::List data;
  /** strategy parameters */
  Rcpp::DataFrame params;
  
  /** resolved price data frames*/
  Rcpp::DataFrame bids;
  Rcpp::DataFrame asks;
  Rcpp::NumericVector datetimes;
  
  /** performance metrics */
  TPerfs perfs;
  
  /** strategy orders*/
  OrderBook orders;
  
public:
  Strategy() {
  }
  
public:
  Rcpp::DataFrame datum(const char* name) {
    if(!data.containsElementNamed(name))    
      return Rcpp::DataFrame();
    return data[name];
  }
  
  void init(Rcpp::List data_, Rcpp::DataFrame params_, Rcpp::List config_) {
    data = data_;
    bids = data["bid"];
    asks = data["ask"];
    datetimes = data["datetime"];
    params = params_;
    config = config_;
    perfs.init(nrows());
  }
  
  int nrows() {
    return bids.nrows();
  }
  
  void on_order_book(const OrderBook &book) {
    
  }
  
  void update_orders(OrderBook &levels) {
    orders.swap(levels);
  }
  
  /*
  void on_order_status(const Order &order) {
    
  }
  
  void send_order(const Order &order) {
    
  }*/
};

template<typename TStrategy> 
class Simulator
{
  Rcpp::List backtest(Rcpp::List data, Rcpp::DataFrame params, Rcpp::List config) {
    int nparams = params.nrows();
    
    Rcpp::DataFrame bids = Rcpp::as<Rcpp::DataFrame>(data["close_bids"]);
    Rcpp::DataFrame asks = Rcpp::as<Rcpp::DataFrame>(data["close_asks"]);
    Rcpp::DataFrame time = Rcpp::as<Rcpp::DataFrame>(data["datetime"]);
    
  
    TStrategy strategy[params.size()];
    for(int i=0; i<params.size(); i++) {
      strategy[i].on_init(data, params, config);
    }
  
      int t = 0;
    int ntime  = bids.nrows();
    
    OrderBook book(2);
    while(t < ntime) {
      book.level(1).price = bids[t];
      book.level(-1).price = asks[t];
      for(int i = 0; i < params.size(); i++) {
        strategy[i].on_order_book(book);
      }
      t++;
    }
  }
};

#endif