#include "backtest.h"

using namespace Rcpp;

class MyAlgorithm : public Algorithm<>
{
public:

};

void test() {
  Simulator simulator;
  DataFrame data;
  DataFrame params;
  List config;
  simulator.backtest<MyAlgorithm>(data, params, config);
}

