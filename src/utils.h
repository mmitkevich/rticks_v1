#ifndef __R_UTILS_H__
#define __R_UTILS_H__

#include "cxx14.h"
#include <spdlog/fmt/ostr.h>
#include <spdlog/spdlog.h>
#include <Rcpp.h>

namespace Rcpp {

extern std::shared_ptr<spdlog::logger> logger;

template<size_t i, typename TVector, typename...Ts>
void _set_nrows(List& data, List &newdata, size_t nrows)
{
  TVector newvec(nrows);
  while(data.size()<i+1)
    data.push_back(TVector());
  TVector vec(as<TVector>(data[i]));
  for(int j=0; j<std::min(vec.size(),newvec.size()); j++)
    newvec[j] = vec[j];
  newdata.push_back(newvec);

  _set_nrows<i+1, Ts...>(data, newdata, nrows);
}

template<size_t i>
void _set_nrows(List& data, List &newdata, size_t nrows)
{
};

template<typename...Ts>
void set_nrows(List& data, size_t nrows) {
  List newdata;
  _set_nrows<0, Ts...>(data, newdata, nrows);
  data = newdata;
}



template<typename TVector> 
TVector get(List config, const char* name, TVector def) {
  return  config.containsElementNamed(name) ? as<TVector>(config[name]) : def;
}


double eps() { return std::numeric_limits<double>::epsilon(); }

bool is_zero(double x)
{
  return std::abs(x) < eps();
}

bool is_equal(double x, double y) {
    return is_zero(x-y) || std::isnan(x) && std::isnan(y);
}

template<typename T>
T required(List list, std::string name, std::string where = "", bool no_clone=false) {
  if(!list.containsElementNamed(name.c_str())) {
    std::stringstream ss;
    ss << "required "<<where<<"$"<<name<<"\n";
    Rcpp::stop(ss.str());
  }
  return no_clone ? as<T>(list[name]) : clone(as<T>(list[name]));
}

template<typename TVector>
TVector optional(List list, std::string name, TVector def=TVector(), bool no_clone=false) {
  if(!list.containsElementNamed(name.c_str())) {
    return def;
  }
  return no_clone ? as<TVector>(list[name]) : clone(as<TVector>(list[name]));
}

template<typename TValue, typename TVector>
TVector optional(DataFrame list, std::string name, TValue def=TValue(), bool no_clone=false) {
  if(!list.containsElementNamed(name.c_str())) {
    return TVector(list.nrows(), def);
  }
  return no_clone ? as<TVector>(list[name]) : clone(as<TVector>(list[name]));
}


template<typename TVector, typename TFunctor> 
void copy_if(TVector &output, TVector &input, TFunctor condition) {
  for(int i=0; i<output.size(); i++) {
    if(condition(input[i]))
      output[i] = input[i];
  }
}



// debug stuff
//extern "C" void breakpoint();
//extern "C" const char* printR(SEXP sexp);

//#define BREAKPOINT breakpoint();

} //namespace
#endif
