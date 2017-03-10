library(rticks)
library(ggplot2)

options(debug=T)

myconfig = list(
  backtest = list(
    log_level=3,
    log_stdout=0,
    log_path="rticks.log",
    freq="days",
    no_cache = F,
    no_clean = F,
    no_save = F
  ))

algo <- "gamma"

# 2016 whole year

#start <- dt(2016)
#stop  <- dt(2016,12,20)

# single chunk
start<-as_datetime("2016-07-20")
stop<-as_datetime("2016-08-17")

params <- data_frame(
  # limits
  buy           = 20,
  sell          = 1000000,
  # initial position
  pos           = 0,
  # take profit
  spread        = 0.25,
  # size to buy on each mpi
  gamma.buy     = 1,
  # size to sell on each mpi
  gamma.sell    = 1,
  # exante prefix
  symbol=         "VIX.CBOE",
  # which month
  active_contract = 6
)
config <- myconfig$backtest
perfs <- params %>% backtest(algo, start=start, stop=stop, config=config)
res <- attr(perfs,"params")
cat("\npnl:\n")
print(res$pnl)
cat("\npos:\n")
print(res$pos)
cat("\nperfs:\n")
print(perfs)

gamma_metrics <- function(perfs) {
  params = attr(perfs, "params")
  
  qtys <- perfs %>% spread(metric, value) %>%  
    select(datetime, symbol,  qty_buy, qty_sell) %>% 
    as_data_frame()
  
  qtys <- qtys %>% transmute(datetime=datetime, 
                             symbol=symbol, 
                             metric="rpnl", 
                             value=pmin(qty_buy, qty_sell)) %>% 
    left_join(params%>%transmute(symbol, spread, multiplier), by="symbol") %>% 
    mutate(value=value*spread*multiplier)
  bind_rows(perfs, qtys) %>% arrange(datetime)
}
perfs <- gamma_metrics(perfs)
perfs %>% plot_bt()
