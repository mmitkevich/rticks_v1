#include "reactive.h"
#include "cxx14.h"

using namespace Rcpp;


template<typename T>
struct Greater {
    typedef double argument_type;
    typedef bool result_type;
    T min;

    Greater(T min): min(min) {

    }

    bool operator()(T x){ return x>min; }
};

void test_from_filter_print_cpp98() {
    std::cout << "\ntest_from_filter_print_cpp98\n"
              << "expected 2,3\n";


    const int n = 3;
    std::vector<double> input;
    for(int i=1; i<=n; i+=1)
        input.push_back(i);

    Print<double> printer(std::cout);
    Filter<Greater<double> > filter(1); // greater than 1 == 2, 3
    From<double, std::vector<double>::iterator> from(input.begin(), input.end());

    from | filter       // means apply filter to from
        >>= printer;    // means "subscribe" printer "on" (from % filter)
}


void test_from_filter_print_cpp11_operators() {
    std::cout << "\ntest_from_filter_print_cpp11_operators\n";

    std::vector<double> input {1, 2, 3};
    auto obs = from(input.begin(), input.end());
    auto obs2 = obs | filter([](double m)$(m>1));
    obs2 >>= println<double>(std::cout);
}

void test_from_map_print_cpp11_operators() {
   std::cout << "\ntest_from_map_print_cpp11_operators\n"
             << "expected: 4, 9, 16\n";

   std::vector<double> input {2, 3};
   input.push_back(4.);
   std::vector<double> output;
   auto obs = from(input.begin(), input.end())
            | map([](double m) $(m*m))
            | into(output)
            >>= println<double>(std::cout, ", ");
}

auto return_observable_auto() {
    return range(1, 10, 1) | filter([](int x)$(x>5));
}

void test_filter_map_copy() {
    std::cout << "\ntest_filter_map_copy\n"
             << "expected: 6 7 8 9\n";
    auto range = return_observable_auto();
    range >>= println<int>(std::cout);

    range | map([](int x)$(x*x))
          | filter([](int x)$(x%2==0))
        >>= println<int>(std::cout);
}


void test_combine() {
   std::cout << "\ntest_combine\n"
                 << "expected: (9,100) (9,105)\n";

    auto obs1 = range(1,10,1);
    auto obs2 = range(100, 110, 5);

    auto two = combine(obs1, obs2);
    std::vector<std::tuple<int,int>> output;
    two  >>= println<decltype(two)::result_type>(std::cout)
         >>= into(output);
}

#ifdef DOES_NOT_WORK

double f1(double x){return x+1;}
double f2(double x){return x*10;}

auto func(auto x) {
    return std::function<decltype(x)>(x);
}

void test_function_compositions()
{
    func(f1) >>= func(f2);
    std::cout << f(1);
}

void test_from_filter_print_cpp14() {
   std::cout << "\ntest_from_filter_print_cpp14\n"
        << "expected: 2,3\n";

   std::vector<double> input {1, 2, 3};
   auto obs = from(input.begin(), input.end());
   auto obs2 = obs.filter([](auto m)$(m>1));
   obs2 >>= println<double>(std::cout);
}
#endif

#ifdef DYNAMIC
void test_dynamic() {
    std::cout <<"test_dynamic\n";
    auto d = as_dynamic(range(1, 10, 1)) >>= reduce([](int x, int y)$(x+y)) >>= println<int>(std::cout);
}
#endif

int main(int argc, char* argv[]) {
    //test_combine();
    //test_from_filter_print_cpp98();
    //test_from_filter_print_cpp11_operators();
    //test_from_map_print_cpp11_operators();
    //test_filter_map_copy();
    //test_from_filter_print_cpp14();
   //     test_dynamic();
    std::cout << "\n";
    return 0;
}

