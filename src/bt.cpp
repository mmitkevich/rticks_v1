#include <Rcpp.h>
#include "utils.h"
#include "metrics.h"


using namespace Rcpp;

typedef const char* symbol_t;

class Simulator;


class GammaAlgorithm {
  NumericVector buy_qty;      // increment in position (in lots) per increment in price ( in MPI )
  NumericVector sell_qty;     
  NumericVector buy;            // maximum price to buy
  NumericVector sell;           // minimum price to sell
  NumericVector mpi;
public:
  template<typename TContext>
  void init(TContext& ctx, DataFrame params) {
     buy_qty = NumericVector(ctx.nsym());
     sell_qty = NumericVector(ctx.nsym());
     buy = as<NumericVector>(params["buy"]);
     sell = as<NumericVector>(params["sell"]);
     mpi = ctx.mpi();
     buy_qty = 1;
     sell_qty = -1;
   }
  
  template<typename TEvent>
  void update(const TEvent &evt) {
    if(evt.fill_qty>0) {  // we bought
      buy[evt.sym] = evt.fill_price - mpi[evt.sym];
    }else if(evt.fill_qty<0) {
      sell[evt.sym] = evt.fill_price + mpi[evt.sym];
    }
  }
};

struct Fill {
  int sym;
  double fill_price;
  double fill_qty;
  
  Fill(int sym_, double fill_qty_ = 0, double fill_price_ = NAN)
    : sym(sym_),
      fill_qty(fill_qty_),
      fill_price(fill_price_) {
  }
};

class Simulator
{
protected:
  NumericVector datetimes_;
  DataFrame bids_df_;
  DataFrame asks_df_;
  int itime_;
  int ntime_;
  int nsym_;
  CharacterVector symbols_;
  NumericVector bids_;
  NumericVector asks_;
  NumericVector high_;
  NumericVector low_;
  NumericVector fill_price_;
  NumericVector fill_qty_;
  NumericVector mpi_;
  Metrics metrics_;
public:

  NumericVector bid(double volume = 0) {
    return  bids_;
  }
    
  NumericVector ask(double volume = 0) {
    return asks_;
  }
  
  NumericVector mpi() {
    return NumericVector();  
  }
  
  
  NumericVector fill_qty() {
    return NumericVector();
  }
  
  NumericVector fill_price() {
    return NumericVector();
  }

  int nsym() {
    return symbols_.size();
  }
  
  symbol_t sym(int isym) {
    return (const char*) symbols_[isym];
  }

  template<typename TEvent>
  void fill(const TEvent &fill) {
      metrics_.fill<TEvent>(fill);
  }
  
  template<typename TAlgorithm>
  void init(
      TAlgorithm &algo, 
      DataFrame params,     // symbol, p1, p2, p3
      List config) 
  {
    algo.init(params);
  }
  
  template<typename TAlgorithm>
  List run( 
      TAlgorithm &algo,
      List data)            // datetime; bid->df<datetime, MSFT, AAPL, ...>; ask->df<datetime, MSFT, AAAPL, ....> 
  {
    datetimes_ = as<NumericVector>(data["datetime"]);
    bids_df_ = as<DataFrame>(data["bid"]);
    asks_df_ = as<DataFrame>(data["ask"]);
    itime_ = 0;
    ntime_ = datetimes_.size();
    nsym_ = bids_df_.size();
    symbols_ = bids_.names();
    //ASSERT(bids.nrows()==asks.nrows()==datetime.size())
    List results;
    while(itime_ < ntime_) {
      update(algo);
      itime_++;
    }
    return results;
  }
  
  template<typename TAlgorithm>
  void update(TAlgorithm &algo) {
    
  }
};

class GammaSimulator : public Simulator {
  typedef Simulator base_type;
  
  template<typename TAlgorithm>
  void fill(TAlgorithm &algo, int isym, double limit_price, double market_price, double qty) {
    Fill evt(isym, market_price, qty);
    algo.update(evt);
    evt.fill_price = 0.5*(market_price + limit_price);
    evt.fill_qty = qty * (fabs(market_price-limit_price)/mpi_[isym] + 1);
    base_type::fill(evt);
  }
  
  template<typename TAlgorithm>
  void update(TAlgorithm &algo) {
    for(int i = 0; i < nsym_; i++) {
      Fill evt(i);
      // TODO: make high, low direction adaptive due to distance (high, close) <> (low, close)
      if(low_[i] < algo.buy_[i]) {
        fill(algo, i, algo.buy_[i], low_[i], algo.buy_qty[i]);
      }
      if(high_[i] > algo.sell_[i]) {
        fill(algo, i, algo.sell_[i], high_[i], algo.sell_qty[i]);
      }
    }
  }
};

