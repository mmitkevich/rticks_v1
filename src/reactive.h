#pragma once

#ifndef REACTIVE_H
#define REACTIVE_H

#include "utils.h"

namespace Rcpp {

template<typename TInput>
struct IObserver {
  typedef TInput argument_type;

  virtual ~IObserver() { }
  virtual void on_next(TInput e) = 0;
  //void operator()(TInput e) {
  //  on_next(e);
  //}
};

template<typename TOutput, typename TObserver> struct IObservable;
template<typename TInput, typename TObserver> struct Observable;
template<typename TInput, typename TOutput, typename TObserver, typename TBase> struct Processor;
template<typename TLeft, typename TRight, typename TOutput, typename TObserver> struct Apply;

template<typename F, typename TOutput, typename TObserver, typename TBase> struct Filter;

template<typename TLeft,
        typename TRight,
        typename TOutput=typename TRight::result_type,   // the output from the obs
        typename TObserver=IObserver<TOutput> >
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
struct ValueObserver : IObserver<TOutput>
{
    TParent *parent;
    TOutput value;
    int count;

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
        typename TObserver=IObserver<TOutput>,
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

  void notify() {
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
    typedef TOutput value_type;
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

template<typename TAlgo>
struct IScheduler {
    virtual ~IScheduler(){}
    virtual void on_schedule(TAlgo* algo, double dt) = 0;
    virtual void on_execute(double dt) = 0;
};

template<typename TOutput,
         typename TObserver=IObserver<TOutput> >
struct Observable : public IObservable<TOutput, TObserver> {
  typedef TObserver observer_type;
  typedef Observable<TOutput,TObserver> this_type;
  typedef std::vector<observer_type*> observers_type;
private:
  observers_type observers;

protected:
  Observable(const Observable &rhs) = default;
  Observable() = default;
public:
  // preprocess output
  virtual void notify(TOutput e) {
    for(typename observers_type::iterator it=observers.begin(); it!=observers.end(); it++) {
        notify(*it, std::move(e));
    }
  }

  virtual void notify(TObserver*obs, TOutput e) {
      obs->on_next(std::move(e));
  }

  void subscribe(TObserver *obs) {
    observers.push_back(obs);
    on_subscribe(obs);
  }

  virtual void on_subscribe(TObserver *obs) {   // TODO: was =0, empty implementation means "hot" observable -- doesn't

  }

//  void operator()(TOutput e) {
//      notify(e);
//  }
};


template<typename TInput,
         typename TOutput,
         typename TObserver=IObserver<TOutput>,
         typename TBase=Observable<TOutput, TObserver> >
struct Processor : public TBase, public IObserver<TInput>
{
    using TBase::notify;
    virtual void on_subscribe(TObserver *obs) { }
    virtual void on_next(TInput e) {
        notify(e);
    }
};


template<typename F,
         typename TInput=typename F::argument_type,
         typename TObserver=IObserver<TInput>,
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
         typename TObserver=IObserver<TOutput>,
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

template<typename TLeft,
         typename TRight>
TLeft operator>>=(TLeft &&left, TRight &&right) {
    left.subscribe(&right);
    return left;
}

/// send value into observable or function
template<typename TObserver,
         typename TInput=typename TObserver::argument_type>
TObserver & operator|=(TInput val, TObserver &fn) {
    fn(std::move(val));
    return fn;
}

template<typename TInput,
         typename TObserver = IObserver<TInput>,
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
         typename TObserver = IObserver<TOutput>,
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
         typename TObserver = IObserver<TOutput>,
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
         typename TObserver=IObserver<TInput>,
         typename TBase=Processor<TInput, TInput, TObserver> >
struct Into : public TBase
{
    using TBase::notify;
    std::vector<TInput> & os;
    Into(std::vector<TInput> &os)
        : os(os)
    {

    }

    virtual void on_next(TInput e) {
        os.push_back(e);
        notify(e);
    }
};

template<typename TInput,
         typename TObserver=IObserver<TInput>,
         typename TBase=Processor<TInput, TInput, TObserver> >
struct Print : public TBase
{
    using TBase::notify;
    std::ostream &os;
    std::string sep;

    Print(const Print &rhs) = default;

    Print(std::ostream &os, std::string sep="\n")
        : os(os), sep(sep)
    {

    }

    virtual void on_next(TInput e) {
        os << e << sep;
        notify(e);
    }

    virtual void on_subscribe(TObserver *obs) { }
};


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

template<typename TInput>
auto println(std::ostream &os, std::string sep="\n") {
    return Print<TInput>(os, sep);
}

template<typename TInput>
auto into(std::vector<TInput> &os) {
    return Into<TInput>(os);
}

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


}; // ns Rcpp

#endif // REACTIVE_H

