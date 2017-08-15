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
};

std::string cwd()
{
   char temp[1024];
   return ( getcwd(temp, sizeof(temp)) ? std::string( temp ) : std::string("") );
}

//' flush spdlog
//'
//' @export
// [[Rcpp::export]]
void flush_spd_log() {
  if(logger){
    std::cout << "log flushed\n";
    logger->flush();
  }
}


//' init_logger
//'
//' init common R/Cpp logger
//' @export
// [[Rcpp::export]]
void init_spd_log(List config) {
    try
    {
        if(logger){
          flush_spd_log();
          logger = nullptr;
        }
        std::string path = (const char*)optional<CharacterVector>(config, "log_path", "rticks.log")[0];
        std::vector<spdlog::sink_ptr> sinks;
        auto log_stdout = optional<IntegerVector>(config, "log_stdout", 0)[0];
        auto log_level = optional<IntegerVector>(config, "log_level", 0)[0];
        
        if(log_stdout!=spdlog::level::off)
          {
            auto stdout_sink = std::make_shared<spdlog::sinks::stdout_sink_st>();
            stdout_sink->set_level((spdlog::level::level_enum)log_stdout);
            sinks.push_back(stdout_sink);
          std::cout << "rticks running in " <<cwd() <<", logging into stdout level " << log_stdout<< std::endl << std::flush;  
        }
        std::cout << " removing log "<<path<<std::endl;
        unlink(path.c_str());
        //sinks.push_back(std::make_shared<spdlog::sinks::daily_file_sink_st>(path, 23, 59));
        if(log_level!=spdlog::level::off)
        {   
          auto file_sink = std::make_shared<spdlog::sinks::simple_file_sink_st>(path);
          file_sink->set_level((spdlog::level::level_enum)log_level);
          sinks.push_back(file_sink);
          std::cout << "rticks running in " <<cwd() <<", logging into " << path 
                    <<" level "<<log_level << std::endl << std::flush;  
          
        }
        ::logger =  std::make_shared<spdlog::logger>("rticks", begin(sinks), end(sinks));
        ::logger->set_pattern("%l   %v");
    }
    catch (const spdlog::spdlog_ex& ex)
    {
        std::cout << "Log initialization failed: " << ex.what() << std::endl;
    }
}

//' spdlog
//'
//' write to spdlog/stdout
//' @export
// [[Rcpp::export]]
void spd_log(IntegerVector lvl, CharacterVector s) {
  std::stringstream ss;
  for(int i=0;i<s.size(); i++) {
    if(i>0)
      ss << " ";
    ss << (const char*)s[i];
  }
  auto l = (spdlog::level::level_enum)lvl[0];
  if(logger)
    logger->log(l, ss.str());
  else
    std::cout << l <<" | " << ss.str() << std::endl;
}


//' bt_gamma
//'
//' will backtest gamma scalping
//' @export
// [[Rcpp::export]]
List bt_gamma(CharacterVector clazz,  List data, List params, List config, List signals) {
  List result;

  if(!logger)
    init_spd_log(config);

  //Rcout << "config=";
  //Rf_PrintValue(config);

  if(clazz[0] == "gamma") {
    Backtester<GammaAlgo<>, GammaSimulator<>> bt(params, config);
    /*
    NewMetrics<> nm(params, config, "nmetrics");
    bt.market.$execs >>= nm;
    nm.pos >>= println<double>(std::cout);*/
    bt.process(data, signals);
    result = bt.market.metrics.toR();
  }
  if(logger)
    logger->flush();
  return result;
}


