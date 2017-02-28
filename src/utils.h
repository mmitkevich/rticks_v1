#ifndef __R_UTILS_H__
#define __R_UTILS_H__

namespace Rcpp {

/** thin wrapper around some indexable data */
template<typename TData, typename TCursor, typename TValue, typename TVector>
struct RowWrapper  {
  TData data;
  TCursor *cur;

  RowWrapper() {
    cur = NULL;
  }
  
  RowWrapper(const TData &data, TCursor *cur) :
    data(data), 
    cur(cur) {
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
  
  int size() const {
    return data.size();
  }
  
  template<typename TColumn> 
  TValue& get(int icol) {
    if(cur==NULL)
      throw new std::range_error("cur==NULL");
    return as<TVector>(data[icol])[cur->index];
  }
};

template<typename TVector> 
TVector get(List config, const char* name, TVector def) {
  return  config.containsElementNamed(name) ? as<TVector>(config[name]) : def;
}


template <typename T>
bool is_zero(T x)
{
  return std::abs(x) < std::numeric_limits<T>::epsilon();
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

template<typename T>
T optional(List list, std::string name, T def = T()) {
  if(!list.containsElementNamed(name.c_str())) {
    return def;
  }
  return as<T>(list[name]);
}


template<typename TVector, typename TFunctor> 
void copy_if(TVector &output, TVector &input, TFunctor condition) {
  for(int i=0; i<output.size(); i++) {
    if(condition(input[i]))
      output[i] = input[i];
  }
}

// debug stuff
extern "C" void breakpoint();
extern "C" const char* printR(SEXP sexp);

#define BREAKPOINT breakpoint();

} //namespace
#endif