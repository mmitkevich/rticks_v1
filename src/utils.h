#ifndef __R_UTILS_H__
#define __R_UTILS_H__

#include "cxx14.h"
#include <Rcpp.h>

namespace Rcpp {

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


bool is_zero(double x)
{
  return std::abs(x) < std::numeric_limits<double>::epsilon();
}

template<typename T>
T required(List list, std::string name, std::string where = "") {
  if(!list.containsElementNamed(name.c_str())) {
    std::stringstream ss;
    ss << "required "<<where<<"$"<<name<<"\n";
    Rcpp::stop(ss.str());
  }
  return as<T>(list[name]);
}

template<typename TVector>
TVector optional(List list, std::string name, TVector def=TVector()) {
  if(!list.containsElementNamed(name.c_str())) {
    return def;
  }
  return as<TVector>(list[name]);
}

template<typename TValue, typename TVector>
TVector optional(DataFrame list, std::string name, TValue def=TValue()) {
  if(!list.containsElementNamed(name.c_str())) {
    return TVector(list.nrows(), def);
  }
  return as<TVector>(list[name]);
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
