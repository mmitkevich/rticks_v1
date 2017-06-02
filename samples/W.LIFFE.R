library(rticks)
library(ggplot2)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  log_level = LOG$INFO,
  log_stdout = LOG$WARN,
  zero_position_freq= F, #as.numeric(months(2)),
  zero_position_on_rolls = F,
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, months_ahead=1 at least 1 month ahead of expiration  
  perfs_freq = as.numeric(days(1)),
  perfs_tz = as.integer(16),
  roll_same_day_all_legs=T
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2011-01-01")
stop  <- as_datetime("2017-05-01")

params <- data_frame(
  # limits
  limit.buy     = 500,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 350,    # FIXME: no buy lower than 18
  risk.buy      = 3200,
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 9,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "W.LIFFE",   # exante prefix of contract series
  
  roll_pattern  = list(list(3,5,10)),
  min_active_contract = 3,
  active_contract = 3             # which month to trade
)

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)
bt_reports(r, save = F)
bt_plot(r, maxpoints = 1000)