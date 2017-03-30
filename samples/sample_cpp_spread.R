library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  log_level = LOG$OFF,
  log_stdout = LOG$INFO, 
  roll_position = F, # if T, then close position roll of ANY instrument (TODO: do it on real roll only). if F - roll position into next contract
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  perfs_freq = as.numeric(days(1))
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2015-01-01")
stop  <- as_datetime("2017-03-01")

params <- data_frame(
  # limits

  limit.buy     = -200,    # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = -360,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 20,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
 
  symbol        = "PL.NYMEX",   # exante prefix of contract series
  weight  = 1,
  roll_pattern  = list(list(4, 10)),
  roll_position = F,
  active_contract = 1             # which month to trade
)

params <- bind_rows(params, data_frame(
  # limits
  
  limit.buy     = NA,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = NA,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 20,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "GC.COMEX",   # exante prefix of contract series
  weight  = -1,
  roll_pattern  = list(list(4, 10)),
  
  active_contract = 1             # which month to trade
))

process_perfs <- function(perfs) {
  results <- attr(perfs,"params")
  data <- attr(perfs,"data")
  
  perfs <- perfs %>% metrics.gamma(results) # calculate additional metrics
  
  
  # view perfs 
  # perfs %>% spread(metric,value) %>% View()
  
  perfs %>% plot_bt()
  
  dir.create("~/rticks_bt", showWarnings=F)
  write.csv(perfs %>% spread(metric, value), file=paste0("~/rticks_bt/",paste.list(params$symbol)))
}

perfs <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)
