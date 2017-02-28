#include <Rcpp.h>
#include <signal.h>
#include "stdout_switcher.h"

using namespace Rcpp;

const char* printR(SEXP sexp)
 {
  static stdout_switcher sw;
  sw.switch_stdout("tmpfile.tmp");
  Rf_PrintValue(sexp);
  return sw.revert_stdout();
}