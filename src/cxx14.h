#pragma once
#ifndef CXX14_H
#define CXX14_H

#include <string>
#include <map>
#include <queue>
#include <cassert>
#include <vector>
#include <iostream>
#include <memory>

// will include tuple, functional
#include "function_traits.h"

//////// tuple stdout stuff
namespace Rcpp {


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



template <typename... X, typename... Y>
auto operator&(const std::tuple<X...>& x, const std::tuple<Y...> &y)
{
    return std::tuple_cat(x,y);
}

template<typename A, typename B, typename C>
auto operator>>(std::function<B(A)> x, std::function<C(B)> y) {
    return [&](A x){return y(x);};
}


template <typename... T>
std::ostream& operator<<(std::ostream& os, const std::tuple<T...>& tup)
{
    os << "[";
    print_tuple<0>(os, tup);
    return os << "]";
}



/////////// vector push_back via <<
//template<typename T>
//inline std::vector<T> & operator<<(std::vector<T> &vec, T val) {
//    vec.push_back(std::move(val));
//    return vec;
//}


////////// lambda short macro syntax

/// purpose of macro to write [](T x)$(x+1) instead of [](T x){return x+1;}
#define $(expr) { return (expr); }

/// purpose of macro to write $$($+1) instead of [](auto $){return $+1;}
#define $$(expr) [](auto $) { return (expr); }


}; //ns

#endif // CXX14_H

