namespace Rcpp {

template<typename TData> 
struct Event
{
  double timestamp;
  TData data;

  Event(TData &data_, double timestamp_) : 
    data(data_),
    timestamp(timestamp_) {

  }
};

} //namespace Rcpp