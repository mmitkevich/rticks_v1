#include <Rcpp.h>
#include <RInside.h>

using namespace Rcpp;
int main(int argc, char* argv[]) {
    RInside R(argc, argv);
    if(argc<2) {
        std::cout << "usage: test_bt script.R\n";
        return 1;
    }
    std::cout << ">>>>>>>>>>>>\n";
    std::string script = "source(\"" + std::string(argv[1]) + std::string("\")");
    std::cout << "============\n";
    R.parseEvalQ(script.c_str());
    std::cout << "<<<<<<<<<<<<\n";
    return 0;
}
