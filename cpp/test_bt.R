library(rticks)
library(ggplot2)

options(debug=T)

myconfig = list(
  backtest = list(
    log_level=0, 
    freq="days",
    no_cache = T,
    no_clean = F,
    no_save = T
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
  buy           = 20.5,
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
results <- attr(perfs,"results")
cat("\npnl:\n")
print(results$pnl)
cat("\npos:\n")
print(results$pos)
cat("\nperfs:\n")
print(perfs)

perfs %>% plot_bt()
