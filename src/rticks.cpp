#include <Rcpp.h>

#include "rticks.h"
#include "gamma.h"

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
    Player<> player(params, config);
    GammaSimulator<> simulator(params, config);
    QuotingAlgo<> algo(params, config);
    Metrics<> metrics(params, config);

    player >>= simulator; // quotes - first to sim (to fill old orders)
    player >>= algo;      // quotes - second to algo (to create new orders)

    simulator >>= algo; // fills
    simulator >>= metrics; // fills

    // send data
    player(data);
  }
  return result;
}

