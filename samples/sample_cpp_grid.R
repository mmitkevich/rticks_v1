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
stop  <- as_datetime("2016-03-03")

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

report <- function(r) {
  # view data
  symbol <- paste0(paste.list(r$params$symbol,"-"))
  r$data %>% bind_rows() %>% View(paste0("data$",symbol))
  
  r$metrics <- r$perfs %>% metrics.gamma(r$params) # calculate additional metrics
  # view perfs 
  r$metrics %>% spread(metric,value) %>% View(paste0("metrics$",symbol))
  
  # plot pnl

  # save results
  dir.create("~/rticks_bt", showWarnings=F)
  dt <- now() %>% strftime("%Y-%m-%d_%H-%M-%S")
  fn <- paste0("~/rticks_bt/",symbol)
  write.csv(r$schedule, file=paste(fn, dt, "schedule.csv", sep="."))
  write.csv(r$metrics %>% spread(metric, value), file=paste(fn, dt, "csv", sep="."))
  r$metrics %>% plot_bt()
}

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)

#report(r)
