library(rticks)
library(ggplot2)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  log_level = LOG$OFF,
  log_stdout = LOG$WARN, 
  zero_position_on_rolls = F,
  zero_position_freq = F,
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, months_ahead=1 at least 1 month ahead of expiration  
  perfs_freq = as.numeric(minutes(1))
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2015-01-01")
stop  <- as_datetime("2017-06-23")

params <- data_frame(
  # limits
  limit.buy     = 0.25,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 0.13,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = +Inf,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.015,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "GE.CME.CS12M",   # exante prefix of contract series
  
  roll_pattern  = list(list(12)),
  min_active_contract = 6,
  active_contract = 6             # which month to trade
)

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg) 
bt_reports(r)
bt_plot(r)