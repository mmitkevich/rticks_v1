namespace Rcpp {

struct Perfs {
  typedef RowWrapper<DataFrame, Perfs, double> NumericMetric;
  
  List result;
  CharacterVector symbols;
  int itime;
  int ntime;
  NumericMetric time;
  NumericMetric max;  // by symbol
  NumericMetric dd;
  NumericMetric mdd;
  NumericMetric pos;
  NumericMetric cash;
  NumericMetric buy_volume;
  NumericMetric sell_volume;
  NumericMetric roundtrips;
  NumericMetric fill_price;
  

  
public:
  
  Perfs() {
    itime = 0;
    ntime = 0;
  }
  
  NumericMetric make_metric(const char *name, double def = NAN) {
    List lst;
    
    for(int i=0; i<ncol(); i++) {
      lst[(const char*)symbols[i]] = NumericVector(ntime);
    }
    result[name] = lst;
    return NumericMetric(DataFrame(lst), this);
  }
  
  int ncol() {
    return symbols.size();  
  }
  
  int nrow() {
    return ntime;
  }

  double& current_value(DataFrame &df) {
    return as<NumericVector>(df)[itime];
  }

  void init(int ntime_, CharacterVector symbols_) {
    ntime = ntime_;
    symbols = symbols_;
    max = make_metric("max");
    dd  = make_metric("dd");
    mdd = make_metric("mdd");
    pos = make_metric("pos");
    cash = make_metric("cash");
    fill_price = make_metric("fill_price");
  }
  
  void on_bar() {
    itime++;
  }
  
  void on_trade(long sym, double volume, double price) {
    pos[sym] += volume;
    cash[sym] -= volume * price;
    (volume > 0 ? buy_volume : sell_volume)[sym] += fabs(volume);
    
    if(is_zero(pos[sym]))
      roundtrips[sym]++;
    
    fill_price[sym] = price;
    //    std::cout << t << " | " << q << " * " << price << " | " << pos << " | " << cash << "\n";
  }
  
};

} //namespace Rcpp