#include "rticks.h"


// *.cpp renamed to *.hpp for @Unity Build


namespace Rcpp {
    int Messages_Constructed = 0;
};

using namespace Rcpp;
template<int mask, typename TInput> 
struct Flags {
  typedef TInput argument_type;
  bool operator()(const argument_type &e) const { return e.get_flag(mask); }
};

//' bt_gamma
//'
//' will backtest gamma scalping
//' @export
// [[Rcpp::export]]
List bt_gamma(CharacterVector clazz,  List data, List params, List config) {
  List result;


  if(clazz[0] == "gamma") {
    Backtester<GammaAlgo<>, GammaSimulator<>> bt(params, config);
    bt.process(data);
    result = bt.market.metrics.toR();
  }
  return result;
}

