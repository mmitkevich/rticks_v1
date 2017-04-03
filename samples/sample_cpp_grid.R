library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  log_level = LOG$OFF,
  log_stdout = LOG$WARN, 
  roll_position = F, # if T, then close position roll of ANY instrument (TODO: do it on real roll only). if F - roll position into next contract
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  perfs_freq = as.numeric(days(1))
))

# init logging, see rticks.log
init_spd_log(cfg)

start <- as_datetime("2015-01-01")
stop  <- as_datetime("2016-03-01")

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
  
  active_contract = 1#seq(2,3)            # which month to trade
)

params <- bind_rows(params, data_frame(
  # limits
  
  limit.buy     = NA,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = NA,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.2,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "GC.COMEX",   # exante prefix of contract series
  weight  = -1,
  roll_pattern  = list(list(4, 10)),
  
  active_contract = 1#seq(2,3)             # which month to trade
))

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg) 
r %>% bt_reports()
# bt_view_metrics(r, start="2015-09-23", stop="2015-09-25")
