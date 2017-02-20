#ifndef __R_UTILS_H__
#define __R_UTILS_H__
#include <Rcpp.h>

namespace Rcpp {

/** thin wrapper around some indexable data */
template<typename TData, typename TCursor, typename TValue>
struct RowWrapper  {
  TData data;
  TCursor *cur;

  RowWrapper() {
    cur = NULL;
  }
  
  RowWrapper(const TData &data_, TCursor *cur_) :
    data(data_), 
    cur(cur_) {
  }
  
  RowWrapper(const RowWrapper &rhs) :
    data(rhs.data),
    cur(rhs.cur) {
        
  }

  RowWrapper& operator=(const RowWrapper &rhs)
  {
    data = rhs.data;
    cur = rhs.cur;
    return *this;
  }

  TValue& operator[](int icol) {
    return get<TValue>(icol);
  }
  
  template<typename TColumn=TValue> 
  TColumn& get(int icol) {
    if(cur==NULL)
      throw new std::range_error("cur==NULL");
    return cur->current_value(data);
  }
};

template<typename TValue=double, typename TVector=Rcpp::NumericVector> 
TValue config(Rcpp::List config, const char* name, TValue def) {
  return  config.containsElementNamed(name) ? (TValue) Rcpp::as<TVector>(config[name])[0] : def;
}


template <typename T>
bool is_zero(T x)
{
  return std::abs(x) < std::numeric_limits<T>::epsilon();
}

} //namespace
#endif