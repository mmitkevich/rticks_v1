// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// bt_gamma
List bt_gamma(CharacterVector clazz, List data, List params, List config);
RcppExport SEXP rticks_bt_gamma(SEXP clazzSEXP, SEXP dataSEXP, SEXP paramsSEXP, SEXP configSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type clazz(clazzSEXP);
    Rcpp::traits::input_parameter< List >::type data(dataSEXP);
    Rcpp::traits::input_parameter< List >::type params(paramsSEXP);
    Rcpp::traits::input_parameter< List >::type config(configSEXP);
    rcpp_result_gen = Rcpp::wrap(bt_gamma(clazz, data, params, config));
    return rcpp_result_gen;
END_RCPP
}
