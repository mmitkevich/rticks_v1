#pragma once

#ifndef REACTIVE_H
#define REACTIVE_H

#include <functional>

#define HAS_AUTO

#ifdef HAS_AUTO
#include "function_traits.h"
#endif

namespace Rcpp {

template<typename TInput>
struct Observer {
  typedef TInput argument_type;

  virtual ~Observer() { }
  virtual void on_next(TInput e) = 0;
  void operator()(TInput e) {
    on_next(e);
  }
};

template<typename TOutput, typename TObserver> struct IObservable;
template<typename TInput, typename TObserver> struct Observable;
template<typename TInput, typename TOutput, typename TObserver, typename TBase> struct Processor;
template<typename TLeft, typename TRight, typename TOutput, typename TObserver> struct Apply;

template<typename F, typename TOutput, typename TObserver, typename TBase> struct Filter;

template<typename TLeft,
        typename TRight,
        typename TOutput=typename TRight::result_type,   // the output from the obs
        typename TObserver=Observer<TOutput> >
struct Apply : IObservable<TOutput, TObserver>
{
  typedef typename TRight::observer_type observer_type;

  TLeft left;
  TRight right;

  Apply(TLeft left, TRight right )
    : left(std::move(left)),
      right(std::move(right))
  {  }

  virtual void subscribe(TObserver *obs) {
      right.subscribe(obs);
      left.subscribe(&right);
  }
};

template<typename TOutput, typename TParent>
struct ValueObserver : Observer<TOutput>
{
    int count;
    TOutput value;
    TParent *parent;

    ValueObserver(TParent *p, TOutput value=0)
        : parent(p),
          value(value),
          count(0) { }

    virtual void on_next(TOutput e) {
        value = e;
        count++;
        parent->notify();
    }
};

template<typename TLeft,
        typename TRight,
        typename TOutput,   // the output from the obs
        typename TObserver=Observer<TOutput>,
        typename TBase=Observable<TOutput, TObserver> >
struct Combine: public TBase
{
  typedef TObserver observer_type;
  typedef Combine<TLeft,TRight,TOutput,TObserver,TBase> observable_type;
  using TBase::notify;
  TLeft left;
  TRight right;
  typedef typename TLeft::result_type left_type;
  typedef typename TRight::result_type right_type;
  ValueObserver<left_type, observable_type> left_value;
  ValueObserver<right_type, observable_type> right_value;

  Combine(TLeft left, TRight right )
    : left(std::move(left)),
      right(std::move(right)),
      left_value(this), right_value(this)
  {  }

  virtual void notify() {
    if(left_value.count*right_value.count>0) {
        TBase::notify(TOutput(left_value.value, right_value.value));
    }
  }

  virtual void on_subscribe(TObserver *obs) {
    left.subscribe(&left_value);
    right.subscribe(&right_value);
  }
};

template<typename TOutput,
         typename TObserver>
struct IObservable {
    typedef TOutput result_type;
    typedef TObserver observer_type;
    //virtual void on_subscribe(TObserver *obs) = 0;
    virtual void subscribe(TObserver *obs) = 0;
    virtual ~IObservable(){ }
#ifdef DOES_NOT_WORK
    auto filter(std::function<bool(TOutput)> f);

    template<typename TResult>
    auto map(std::function<TResult(TOutput)> f);
#endif
    //Apply<TObservable, Filter<F, TOutput, TObserver, TObservable >, TOutput, TOutput, TObserver> filter(F f);
};

template<typename TOutput,
         typename TObserver>
struct Observable : public IObservable<TOutput, TObserver> {
  typedef TObserver observer_type;
  typedef Observable<TOutput,TObserver> this_type;
  typedef std::vector<observer_type*> observers_type;
  observers_type observers;
protected:
  Observable(const Observable &rhs) = default;
  Observable() = default;
public:
  // preprocess output
  virtual void notify(TOutput e) {
    for(typename observers_type::iterator it=observers.begin(); it!=observers.end(); it++)
      notify(*it, e);
  }

  virtual void notify(TObserver*obs, TOutput e) {
      obs->on_next(e);
  }

  void subscribe(TObserver *obs) {
    observers.push_back(obs);
    on_subscribe(obs);
  }

  virtual void on_subscribe(TObserver *obs) {   // TODO: was =0, empty implementation means "hot" observable -- doesn't

  }
};


template<typename TInput,
         typename TOutput,
         typename TObserver=Observer<TOutput>,
         typename TBase=Observable<TOutput, TObserver> >
struct Processor : public TBase, public Observer<TInput>
{
    virtual void on_subscribe(TObserver *obs) { }
};


template<typename F,
         typename TInput=typename F::argument_type,
         typename TObserver=Observer<TInput>,
         typename TBase=Processor<TInput, TInput, TObserver> >
struct Filter : public TBase
{
  typedef TInput result_type;
  typedef TInput argument_type;
  using TBase::notify;

  F fn;

  Filter(const Filter &rhs) = default;

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
         typename TBase=Processor<TInput, TOutput, TObserver> >
struct Map : public TBase
{
  typedef TOutput result_type;
  typedef TInput argument_type;
  using TBase::notify;
  F fn;

  Map(const Map &rhs) = default;

  Map(const F &fn = F())
    : fn(fn)
  { }

  virtual void on_next(TInput e) {
    notify(fn(e));
  }
};


template<typename TLeft,
         typename TRight>
Apply<TLeft,TRight> operator%(TLeft left, TRight right) {
    return Apply<TLeft, TRight>(std::move(left), std::move(right));
}

#ifdef HAS_AUTO
template<typename TLeft,
         typename TRight>
TLeft operator>>=(TLeft && left, TRight && right) {
    left.subscribe(&right);
    return left;
}
#endif

/// send value into observable or function
template<typename TObserver,
         typename TInput=typename TObserver::argument_type>
TObserver & operator|=(TInput val, TObserver &fn) {
    fn(val);
    return fn;
}

template<typename TInput,
         typename TObserver = Observer<TInput>,
         typename TBase = Processor<TInput, TObserver> >
struct Buffer : public TBase
{
    using TBase::notify;
    std::deque<TInput> buffer;

    Buffer(const Buffer &rhs) = default;

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
         typename TBase = Observable<TOutput, TObserver> >
struct From : public TBase
{
    using TBase::notify;

    TInputIterator curr;
    TInputIterator end;

    From() { };
    From(const From &rhs) = default;

    From(TInputIterator begin, TInputIterator end)
        : curr(begin),
          end(end) { }

    virtual void on_subscribe(TObserver *obs) {
        for(;curr!=end; curr++) {
            TOutput e = *curr;
            notify(e);
        }
    }
};

template<typename TOutput,
         typename TStep = TOutput,
         typename TObserver = Observer<TOutput>,
         typename TBase = Observable<TOutput, TObserver> >
struct FromRange: public TBase
{
    using TBase::notify;
    TOutput begin;
    TOutput end;
    TStep step;

    FromRange(TOutput begin, TOutput end, TStep step)
        : begin(begin),
          end(end),
          step(step) { }

    virtual void on_subscribe(TObserver *obs) {
        for(; begin!=end; begin+=step)
            notify(begin);
    }
};


template<typename TInput,
         typename TStream = std::ostream,
         typename TObserver=Observer<TInput>,
         typename TBase=Processor<TInput, TInput, TObserver> >
struct Into : public TBase
{
    using TBase::notify;
    TStream &os;
    Into(TStream &os)
        : os(os)
    {

    }

    virtual void on_next(TInput e) {
        os << e;
        notify(e);
    }
};

template<typename TInput,
         typename TStream = std::ostream,
         typename TObserver=Observer<TInput>,
         typename TBase=Processor<TInput, TInput, TObserver> >
struct Print : public TBase
{
    using TBase::notify;
    TStream &os;
    std::string sep;

    Print(const Print &rhs) = default;

    Print(TStream &os, std::string sep="\n")
        : os(os), sep(sep)
    {

    }

    virtual void on_next(TInput e) {
        os << e << sep;
        notify(e);
    }

    virtual void on_subscribe(TObserver *obs) { }
};


#ifdef HAS_AUTO

namespace detail {
    template <size_t n, typename... T>
    typename std::enable_if<(n >= sizeof...(T))>::type
        print_tuple(std::ostream&, const std::tuple<T...>&)
    {}

    template <size_t n, typename... T>
    typename std::enable_if<(n < sizeof...(T))>::type
        print_tuple(std::ostream& os, const std::tuple<T...>& tup)
    {
        if (n != 0)
            os << ", ";
        os << std::get<n>(tup);
        print_tuple<n+1>(os, tup);
    }
};

template <typename... T>
std::ostream& operator<<(std::ostream& os, const std::tuple<T...>& tup)
{
    os << "[";
    detail::print_tuple<0>(os, tup);
    return os << "]";
}

template<typename TInputIterator,
         typename TOutput=typename TInputIterator::value_type>
auto from(TInputIterator begin, TInputIterator end) {
    From<TOutput, TInputIterator> result(begin, end);
    return result;
}

template<typename TOutput, typename TStep=TOutput>
auto range(TOutput begin, TOutput end, TStep step) {
    FromRange<TOutput> result(begin, end, step);
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

template<typename L, typename R>
auto combine(L left, R right) {
    return Combine<L,R,std::tuple<typename L::result_type,typename R::result_type>>(left, right);
}

template<typename TInput,
         typename TStream>
auto println(TStream &os, std::string sep="\n") {
    return Print<TInput, TStream>(os, sep);
}

template<typename TInput>
auto into(std::vector<TInput> &os) {
    return Into<TInput, std::vector<TInput> >(os);
}

template<typename A, typename B, typename C>
auto operator>>(std::function<B(A)> x, std::function<C(B)> y) {
    return [&](A x){return y(x);};
}

/// purpose of macro to write [](T x)$(x+1) instead of [](T x){return x+1;}
#define $(expr) { return (expr); }

/// purpose of macro to write $$($+1) instead of [](auto $){return $+1;}
#define $$(expr) [](auto $) { return (expr); }
#endif


#ifdef DOES_NOT_WORK
template<typename TOutput, typename TObserver>
template<TThis>
auto IObservable<TOutput, TObserver>::filter(std::function<bool(TOutput)> f) {
    return Apply<TThis, Filter<decltype(f)>>(*this, Filter<decltype(f)>(f));
}

template<typename TOutput, typename TObserver, typename TThis>
template<typename TResult, typename TThis>
auto IObservable<TOutput, TObserver, TThis>::map(std::function<TResult(TOutput)> f) {
    return Apply<TThis, Filter<decltype(f)>>(*this, Filter<decltype(f)>(f));
}

#endif

#if 1
template<typename T>
inline std::vector<T> & operator<<(std::vector<T> &vec, T val) {
    vec.push_back(std::move(val));
    return vec;
}
#endif
}; // ns Rcpp

#endif // REACTIVE_H

