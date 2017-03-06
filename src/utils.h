#ifndef __R_UTILS_H__
#define __R_UTILS_H__

#include "cxx14.h"
#include <Rcpp.h>

namespace Rcpp {

/** thin wrapper around some indexable data */
template<typename TValue, typename TVector, typename TData=List>
struct DFRow  {
  TData data;
  size_t *cur;
  TValue na;
  std::string name;
  CharacterVector names;

  DFRow() : cur(NULL) { }

  struct Value {
      TVector vec;
      size_t cur;
      Value(const TVector &vec, size_t cur):
          vec(std::move(vec)), cur(cur) {

      }

      Value() = default;

      operator TValue() {
        return vec[cur];
      }

      Value& operator=(TValue v) {
          vec[cur] = v;
          return *this;
      }

      Value& operator+=(TValue v) {
          vec[cur] = vec[cur] + v;
          return *this;
      }

      Value& operator-=(TValue v) {
          vec[cur] = vec[cur] + v;
          return *this;
      }

      Value& operator++() {
          vec[cur] = vec[cur] + 1;
          return *this;
      }
      Value& operator--() {
          vec[cur] = vec[cur] - 1;
          return *this;
      }
  };

  DFRow(CharacterVector names, size_t nrows, TValue na, size_t* cur=NULL)
    : cur(cur),
      na(na),
      names(names),
      data(names.size())
  {
    for(int i=0; i<names.size(); i++) {
        data[i] = TVector(nrows, na);
    }
  }
  
  DFRow(DFRow &&rhs) = default;

  DFRow& operator=(DFRow &&rhs) {
      data = std::move(rhs.data);
      cur = rhs.cur;
      na = rhs.na;
      name = std::move(rhs.name);
  }

  Value operator[](int icol) {
    return Value(as<TVector>(data[icol]), *cur);
  }
  
  int size() const {
    return data.size();
  }

  void set(int i, TValue value) {
    as<TVector>(data[i])[*cur] = value;
  }

  TValue get(int i, int lag=0) {
    return as<TVector>(data[i])[*cur-lag];
  }

  void set(TVector v) {
    for(int i=0; i<size(); i++)
        set(i, v[i]);
  }

  void nrows(size_t nrows) {
    TData newdata(data.size());
    for(int i=0; i<data.size(); i++) {
        TVector newvec(nrows);
        TVector vec(data[i]);
        for(int j=0; j<newvec.size(); j++)
            newvec[j] = vec[j];
        newdata[i] = newvec;
    }
    data = newdata;
  }
};

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
