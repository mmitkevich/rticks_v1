#include "rticks.h"


// *.cpp renamed to *.hpp for @Unity Build


namespace Rcpp {
    int Messages_Constructed = 0;
};

using namespace Rcpp;
template<int mask, typename TInput> 
struct Flags {
  typedef TInput argument_type;
  bool operator()(const argument_type &e) const { return e.get_flag(mask); }
};

namespace Rcpp {

std::shared_ptr<spdlog::logger> logger;

std::string cwd()
{
   char temp[1024];
   return ( getcwd(temp, sizeof(temp)) ? std::string( temp ) : std::string("") );
}

void init_logger(List config) {
    try
    {
        std::vector<spdlog::sink_ptr> sinks;
        if(optional<NumericVector>(config, "log_stdout", 0)[0]>0)
            sinks.push_back(std::make_shared<spdlog::sinks::stdout_sink_st>());
        std::string path = (const char*)optional<CharacterVector>(config, "log_path", "rticks.log")[0];
        //unlink(path.c_str());
        //sinks.push_back(std::make_shared<spdlog::sinks::daily_file_sink_st>(path, 23, 59));
        sinks.push_back(std::make_shared<spdlog::sinks::simple_file_sink_st>(path));
        std::cout << "dticks running in " <<cwd() <<", logging into " << path << std::endl << std::flush;
        ::logger =  std::make_shared<spdlog::logger>("rticks", begin(sinks), end(sinks));
        ::logger->set_pattern("%v");
    }
    catch (const spdlog::spdlog_ex& ex)
    {
        std::cout << "Log initialization failed: " << ex.what() << std::endl;
    }
}

};

//' bt_gamma
//'
//' will backtest gamma scalping
//' @export
// [[Rcpp::export]]
List bt_gamma(CharacterVector clazz,  List data, List params, List config) {
  List result;

  init_logger(config);

  if(clazz[0] == "gamma") {
    Backtester<GammaAlgo<>, GammaSimulator<>> bt(params, config);
    /*
    NewMetrics<> nm(params, config, "nmetrics");
    bt.market.$execs >>= nm;
    nm.pos >>= println<double>(std::cout);*/
    bt.process(data);
    result = bt.market.metrics.toR();
  }
  logger->flush();
  return result;
}


