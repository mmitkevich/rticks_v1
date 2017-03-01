#include <Rcpp.h>
#include "events.h"

using namespace Rcpp;

struct Greater {
    typedef double argument_type;
    typedef bool result_type;
    bool operator()(double x){ return x>1; }
};

int main1(int argc, char* argv[]) {
    double arr[3] = {1, 2, 3};
    std::vector<double> input(3);

    for(int i=0;i<3;i++)
        input.push_back(arr[i]);

    Print<double> printer(std::cout);
    Filter<Greater, double> filter;
    From<double, std::vector<double>::iterator> from(input.begin(), input.end());

    from >>= filter >>= printer;

    //player >> Filter<Flags<Message::FROM_MARKET, QuotesUpdated> >() >> Print(stdout);

    return 0;
}

int Messages_Constructed;



int main(int argc, char* argv[]) {
    typedef std::vector<Message> Messages;
    Messages input;
    for(int i=0;i<3;i++)
        input.push_back(Message(1.0*i));
#if 0
    Print<Message> printer;
    Filter<Greater, Message> filter;
    From<Messages> from;
    input >> from | filter | printer;
#else
    std::function<bool(Message)> f = [](Message m)->bool {return m>2;};

    from(input) >>= (filter([](Message m){
                         return m>0;
                     }) >>= print<Message>(std::cout));
#endif

    //player >> Filter<Flags<Message::FROM_MARKET, QuotesUpdated> >() >> Print(stdout);

    return 0;
}
