#pragma once

#ifndef REACTIVE_H
#define REACTIVE_H

#define HAS_AUTO

#include <functional>
#ifdef HAS_AUTO
#include "function_traits.h"
#endif

namespace Rcpp {

template<typename TInput>
struct Observer {
  typedef TInput argument_type;
  virtual void on_next(TInput e) {};
  void operator()(TInput e) {
    on_next(e);
  }
};


template<typename TOutput,
         typename TObserver=Observer<TOutput> >
struct Observable {
  typedef TOutput result_type;
  typedef TObserver observer_type;
  typedef std::vector<observer_type*> observers_type;
  observers_type observers;

  Observable() {
  }

  // preprocess output
  virtual void notify(TOutput &e) {
    for(int i=0; i<observers.size(); i++)
      notify(observers[i], e);
  }

  virtual void notify(TObserver*obs, TOutput &e) {
      obs->on_next(e);
  }

  void subscribe(TObserver *obs) {
    observers.push_back(obs);
    on_subscribe(obs);
  }

  virtual void on_subscribe(TObserver *obs) {

  }

};

template<typename TInput,
         typename TOutput,
         typename TObserver=Observer<TOutput>,
         typename TObservable=Observable<TOutput, TObserver> >
struct Processor : public TObservable, public Observer<TInput>
{
};


template<typename F,
         typename TInput=typename F::argument_type,
         typename TObserver=Observer<TInput>,
         typename TObservable=Processor<TInput, TInput, TObserver> >
struct Filter : public TObservable
{
  typedef TInput result_type;
  typedef TInput argument_type;
  using TObservable::notify;

  F fn;

  Filter(const F &fn = F())
    : fn(fn)
  {  }

  virtual void on_next(TInput e) {
    if(fn(e))
      notify(e);
  }
};

template<typename F,
         typename TInput=typename F::argument_type,
         typename TOutput=typename F::result_type,
         typename TObserver=Observer<TOutput>,
         typename TObservable=Processor<TInput, TOutput, TObserver> >
struct Map : public TObservable
{
  F fn;

  Map(const F &fn = F())
    : fn(fn)
  { }

  virtual void on_next(TInput e) {
    notify(fn(e));
  }
};

/*
template< typename TLeft,
          typename TRight,
          typename TInput=typename TLeft::argument_type,
          typename TOutput=typename TRight::result_type,
          typename TObserver=typename TRight::observer_type,
          typename TObservable=Processor<TInput, TOutput, TObserver> >
struct Apply : TObservable
{
  typedef typename TRight::observer_type observer_type;
  using TObservable::notify;
  typedef TObservable base;

  TLeft left;
  TRight right;

  Apply(const TLeft &left, const TRight &right )
    : left(left), right(right)
  {
     left.subscribe(&right);
     right.subscribe(this);
  }

  virtual void on_next(const TInput& e) {
    left.on_next(e);
  }

  virtual void subscribe(observer_type *obs) {
      base::subscribe(obs);
  }
};


template<typename TLeft,
         typename TRight>
Apply<TLeft,TRight> operator|(const TLeft &left, const TRight &right) {
  return Apply<TLeft, TRight>(left, right);
}

template<typename TVector,
         typename TObserver>
TObserver & operator>>=(const TVector & input, TObserver right) {
  typedef typename TVector::const_iterator iterator_t;
  for(iterator_t it = input.begin(); it!=input.end(); it++)
      right.on_next(*it);
  return right;
}
*/

template<typename TLeft,
         typename TRight>
TLeft & operator>>=(TLeft & left, TRight & right) {
    left.subscribe(&right);
    return left;
}

template<typename TLeft,
         typename TRight>
TLeft operator>>=(TLeft && left, TRight && right) {
    auto result(left);
    left.subscribe(&right);
    return left;
}

/// send value into observable or function
template<typename TObserver,
         typename TInput=typename TObserver::argument_type>
TObserver & operator|=(TInput val, TObserver &fn) {
    fn(val);
    return fn;
}

template<typename TInput,
         typename TObserver = Observer<TInput>,
         typename TObservable = Processor<TInput, TObserver> >
struct Buffer : public TObservable
{
    using TObservable::notify;
    std::deque<TInput> buffer;

    Buffer() {

    }

    template<typename TInputIterator>
    Buffer(TInputIterator begin, TInputIterator end)
        : buffer(begin, end) { }

    virtual void on_next(TInput e) {
        buffer.insert(buffer.end(), e.begin(), e.end());
        while(buffer.size()>0){
            notify(buffer.pop_front());
        }
    }

    virtual void on_subscribe(TObserver *obs) {
        while(buffer.size()>0) {
            notify(buffer.pop_front());
        }
    }
};

template<typename TOutput,
         typename TInputIterator,
         typename TObserver = Observer<TOutput>,
         typename TObservable = Observable<TOutput, TObserver> >
struct From : public TObservable
{
    using TObservable::notify;
    TInputIterator begin;
    TInputIterator end;

    template<typename TIterable>
    From(TIterable input)
        : From(input.begin(), input.end()) {   }

    From(TInputIterator begin, TInputIterator end)
        : begin(begin),
          end(end) { }

    virtual void on_subscribe(TObserver *obs) {
        for(;begin!=end; begin++) {
            notify(*begin);
        }
    }
};


template<typename TInput,
         typename TStream = std::ostream,
         typename TObserver=Observer<TInput> >
struct Print : public Processor<TInput, TInput, TObserver>
{
    using Observable<TInput>::notify;
    TStream &os;
    std::string sep;
    Print(TStream &os, std::string sep = "\n")
        : os(os), sep(sep)
    {

    }

    virtual void on_next(TInput e) {
        os << e << sep;
        notify(e);
    }
};

#ifdef HAS_AUTO
template<typename TVector,
         typename TOutput=typename TVector::value_type>
auto from(TVector x) {
    From<TOutput, typename TVector::iterator> result(x.begin(), x.end());
    return result;
}

template<typename F>
auto filter(F f) {
    using argtype = typename function_traits<F>::template argument_type<0>;
    return Filter<F,  argtype >(f);
}

template<typename F>
auto map(F f) {
    using argtype = typename function_traits<F>::template argument_type<0>;
    using restype = typename function_traits<F>::result_type;
    return Map<F,  argtype, restype >(f);
}

template<typename TInput,
         typename TStream>
auto print(TStream &os) {
    return Print<TInput, TStream>(os);
}

template<typename A, typename B, typename C>
auto operator>>=(std::function<B(A)> x, std::function<C(B)> y) {
    return [&](A x){return y(x);};
}

#endif

}; // ns Rcpp

#endif // REACTIVE_H

