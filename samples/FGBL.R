library(rticks)
library(ggplot2)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  log_level = LOG$DEBUG,
  log_stdout = LOG$WARN,
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  perfs_freq = as.numeric(minutes(1))
))

# init logging, see rticks.log
init_spd_log(cfg)

start <- as_datetime("2015-01-01")
stop  <- as_datetime("2015-01-10")

params <- data_frame(
  # limits
  
  limit.buy     = -2,    # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = -7,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = +Inf,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.25,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "FGBL.EUREX",   # exante prefix of contract series
  weight  = 1,
  #roll_pattern  = list(list(4, 10)),
  min_active_contract = 1,
  active_contract = 1
)

params <- bind_rows(params, data_frame(
  # limits
  
  limit.buy     = -Inf,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = -Inf,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = +Inf,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.2,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  
  symbol        = "CONF.EUREX",   # exante prefix of contract series
  weight  = -1,
  #roll_pattern  = list(list(4, 10)),
  min_active_contract = 1,
  active_contract = 1
))

LowRisk <- -400

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)
bt_reports(r)