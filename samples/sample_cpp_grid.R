library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = F, # не писать кэш на диск
  log_level = LOG$DEBUG,
  log_stdout = LOG$WARN, 
  zero_position_on_rolls = F,
  zero_position_freq = F, #as.numeric(months(2)), 
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  perfs_freq = as.numeric(minutes(1)),
  perfs_tz = as.integer(16)
))

# init logging, see rticks.log
init_spd_log(cfg)

start <- as_datetime("2015-01-01")
stop  <- as_datetime("2017-01-10")

params <- data_frame(
  # limits
  
  limit.buy     = -200,    # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 20,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "PL.NYMEX",   # exante prefix of contract series
  weight  = 1,
  roll_pattern  = list(list(4, 10)),
  min_active_contract = 1,
  active_contract = 2
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
  min_active_contract = 1,
  active_contract = 1
))



r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg) 
bt_reports(r)
#bt_view_metrics(r, start="2015-09-23 15:48:00", stop="2015-09-25")
#r$metrics %>% filter(pos!=lag(pos) | pos!=lead(pos)|bid!=lag(bid)|ask!=lag(ask)|bid!=lead(bid)|ask!=lead(ask)) %>% filter_date("2016-01-01","2016-05-01") %>% View()
bt_plot(r,no_gaps=F)
print(bt_summaries(r))

#r$metrics %>% filter_date("2015-12-18") %>% filter(bid!=lead(bid)|ask!=lead(ask)|pos!=lead(pos)) %>% View()
