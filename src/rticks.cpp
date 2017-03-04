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
    Player<GammaMessage> player(params, config); // player acts like scheduler / latency simulator
    GammaSimulator<GammaMessage> market(params, config);
    GammaAlgo<GammaMessage> algo(params, config);
    Metrics<> metrics(params, config);

    // wire up market
    player.$quotes >>= market;
    player.$orders >>= market;
    market.$execs >>= player.$execs; // will delay them

    // wire up algo
    player.$quotes >>= algo;
    player.$execs >>= algo;
    algo.$orders >>= player.$orders;

    // wire up metrics
    player.$execs >>= metrics;

    // send data
    player.process(data);

    result = metrics.toR();
  }
  return result;
}

