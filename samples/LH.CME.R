library(rticks)
library(ggplot2)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  log_level = LOG$OFF,
  log_stdout = LOG$WARN,
  roll_position = T, # if T, then close position roll of ANY instrument (TODO: do it on real roll only). if F - roll position into next contract
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, months_ahead=1 at least 1 month ahead of expiration  
  perfs_freq = as.numeric(minutes(1))
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2015-01-22")
stop  <- as_datetime("2017-03-24")

params <- data_frame(
  # limits
  limit.buy     = 76,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 50,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = +Inf,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.8,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "LH.CME",   # exante prefix of contract series
  
  #roll_pattern  = list(list(2,7,10)),
  min_active_contract = 4,
  active_contract = 5             # which month to trade
)
LowRisk <- 40

c("F","G","H","J","K","M","N","Q","U","V","X","Z")
c( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12)

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg) 
bt_reports(r)
r$metrics %>% filter(pos!=lag(pos) | pos!=lead(pos)|bid!=lag(bid)|ask!=lag(ask)|bid!=lead(bid)|ask!=lead(ask)) %>% View()
bt_plot(r)
