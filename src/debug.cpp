#include <Rcpp.h>
#include "stdout_switcher.hpp"
#include <signal.h>

extern "C" const char* printR(SEXP sexp);

const char* printR(SEXP sexp)
{
  static stdout_switcher sw;
  sw.switch_stdout("tmpfile.tmp");
  Rf_PrintValue(sexp);
  return sw.revert_stdout();
}
