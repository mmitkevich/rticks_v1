library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  # no_clean=F,
  # log_level = LOG$INFO,
  # log_stdout = LOG$INFO, 
  # roll_position = F, # if T, then close position roll of ANY instrument (TODO: do it on real roll only). if F - roll position into next contract
  # custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  # perfs_freq = days(1)
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2015-01-01")
stop  <- as_datetime("2017-03-03")

params <- data_frame(
  # limits
  limit.buy     = 76,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 50,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.8,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "LH.CME",   # exante prefix of contract series
  
  # roll_pattern  = list(list(7, 12)),
  
  active_contract = 5             # which month to trade
)

perfs <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)
results <- attr(perfs,"params")
data <- attr(perfs,"data")

perfs <- perfs %>% metrics.gamma(results) # calculate additional metrics


# view perfs 
# perfs %>% spread(metric,value) %>% View()

perfs %>% plot_bt()

dir.create("~/rticks_bt", showWarnings=F)
write.csv(perfs %>% spread(metric, value), file=paste0("~/rticks_bt/",params$symbol))
