#ifndef FUNCTION_TRAITS_H
#define FUNCTION_TRAITS_H

#include <tuple>
#include <functional>

namespace Rcpp {

// Remove the first item in a tuple
template<typename T>
struct tuple_tail;

template<typename Head, typename ... Tail>
struct tuple_tail<std::tuple<Head, Tail ...>>
{
  using type = std::tuple<Tail ...>;
};

// std::function
template<typename FunctionT>
struct function_traits
{
  using arguments = typename tuple_tail<
      typename function_traits<decltype( & FunctionT::operator())>::arguments>::type;

  static constexpr std::size_t arity = std::tuple_size<arguments>::value;

  template<std::size_t N>
  using argument_type = typename std::tuple_element<N, arguments>::type;

  using result_type = typename function_traits<decltype( & FunctionT::operator())>::result_type;
};

// Free functions
template<typename ReturnTypeT, typename ... Args>
struct function_traits<ReturnTypeT(Args ...)>
{
  using arguments = std::tuple<Args ...>;

  static constexpr std::size_t arity = std::tuple_size<arguments>::value;

  template<std::size_t N>
  using argument_type = typename std::tuple_element<N, arguments>::type;

  using result_type = ReturnTypeT;
};

// Function pointers
template<typename ReturnTypeT, typename ... Args>
struct function_traits<ReturnTypeT (*)(Args ...)>: function_traits<ReturnTypeT(Args ...)>
{};

// std::bind for object methods
template<typename ClassT, typename ReturnTypeT, typename ... Args, typename ... FArgs>
#if defined _LIBCPP_VERSION  // libc++ (Clang)
struct function_traits<std::__1::__bind<ReturnTypeT (ClassT::*)(Args ...), FArgs ...>>
#elif defined __GLIBCXX__  // glibc++ (GNU C++)
struct function_traits<std::_Bind<std::_Mem_fn<ReturnTypeT (ClassT::*)(Args ...)>(FArgs ...)>>
#elif defined _MSC_VER  // MS Visual Studio
struct function_traits<
  std::_Binder<std::_Unforced, ReturnTypeT(__cdecl ClassT::*)(Args ...), FArgs ...>
>
#else
#error "Unsupported C++ compiler / standard library"
#endif
  : function_traits<ReturnTypeT(Args ...)>
{};

// std::bind for free functions
template<typename ReturnTypeT, typename ... Args, typename ... FArgs>
#if defined _LIBCPP_VERSION  // libc++ (Clang)
struct function_traits<std::__1::__bind<ReturnTypeT( &)(Args ...), FArgs ...>>
#elif defined __GLIBCXX__  // glibc++ (GNU C++)
struct function_traits<std::_Bind<ReturnTypeT(*(FArgs ...))(Args ...)>>
#elif defined _MSC_VER  // MS Visual Studio
struct function_traits<std::_Binder<std::_Unforced, ReturnTypeT(__cdecl &)(Args ...), FArgs ...>>
#else
#error "Unsupported C++ compiler / standard library"
#endif
  : function_traits<ReturnTypeT(Args ...)>
{};

// Lambdas
template<typename ClassT, typename ReturnTypeT, typename ... Args>
struct function_traits<ReturnTypeT (ClassT::*)(Args ...) const>
  : function_traits<ReturnTypeT(ClassT &, Args ...)>
{};

template<typename FunctionT>
struct function_traits<FunctionT &>: function_traits<FunctionT>
{};

template<typename FunctionT>
struct function_traits<FunctionT &&>: function_traits<FunctionT>
{};

/* NOTE(esteve):
 * VS2015 does not support expression SFINAE, so we're using this template to evaluate
 * the arity of a function.
 */
template<std::size_t Arity, typename FunctorT>
struct arity_comparator : std::integral_constant<
    bool, (Arity == function_traits<FunctorT>::arity)>{};

template<typename FunctorT, typename ... Args>
struct check_arguments : std::is_same<
    typename function_traits<FunctorT>::arguments,
    std::tuple<Args ...>
  >
{};

template<typename FunctorAT, typename FunctorBT>
struct same_arguments : std::is_same<
    typename function_traits<FunctorAT>::arguments,
    typename function_traits<FunctorBT>::arguments
  >
{};

};
#endif // FUNCTION_TRAITS_H

