#pragma once

#include "events.h"

namespace Rcpp {

struct Metrics : Algo {
  typedef RowWrapper<DataFrame, Metrics, double, NumericVector> NumericMetric;
  
  List result;
  CharacterVector symbols;
  
  int index;
  int stop;
  
  NumericMetric time;
  NumericMetric max;  // by symbol
  NumericMetric dd;
  NumericMetric mdd;
  NumericMetric pos;
  NumericMetric rpnl;
  NumericMetric buy_volume;
  NumericMetric sell_volume;
  NumericMetric roundtrips;
  NumericMetric fill_price;

  Metrics(DataFrame params, List config)
    : Algo(params, config),
      index(0),
      stop(0) {
    max = make_metric("pnl"); // summary equity
    dd  = make_metric("dd");
    mdd  = make_metric("mdd");
    pos = make_metric("pos");
    rpnl = make_metric("rpnl"); // realized cash equity
    symbols = required<CharacterVector>(params, "symbols");
  }
  
  NumericMetric make_metric(const char *name, double def = NAN) {
    List lst;
    
    for(int isym=0; isym<symbols.size(); isym++) {
      lst[(const char*)symbols[isym]] = NumericVector(stop);
    }
    result[name] = lst;
    return NumericMetric(this, DataFrame(lst));
  }
  
  void on_next(OrderFilled fill) {
    int i = fill.symbol.index;
    pos[i] += fill.qty;
    rpnl[i] -= fill.qty * fill.price;
    (fill.qty > 0 ? buy_volume : sell_volume)[i] += fabs(fill.qty);
    
    if(is_zero(pos[i]))
      roundtrips[i]++;
    
    //    std::cout << t << " | " << q << " * " << price << " | " << pos << " | " << cash << "\n";
  }
  
};

} //namespace Rcpp
