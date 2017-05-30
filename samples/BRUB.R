library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = F, # не писать кэш на диск
  log_level = LOG$INFO,
  log_stdout = LOG$WARN, 
  zero_position_on_rolls = F,
  zero_position_freq = as.numeric(months(3)),#as.numeric(months(1)), 
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  perfs_freq = as.numeric(days(1)),
  perfs_tz = as.integer(16),
  roll_same_day_all_legs=F,
  mpi=1, # overrides spread's
  multiplier=1000 # overrides spread's
))

# init logging, see rticks.log
init_spd_log(cfg)

start <- as_datetime("2011-01-01")
stop  <- as_datetime("2016-02-20")

params <- data_frame(
  # limits
  
  limit.buy     = 3300,    # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 2100,
  
  risk.buy      = 1500,
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = +Inf,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 50,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "B.ICE",   # exante prefix of contract series
  weight        = 1/1000, # /1000 to go into roubles via currency coefficient Si.FORTS/1000.
  power         = 1,            # for power <0, bid = ask^power, ask=bid^power, for power>0 bid= bid^power, ask= ask^power
  currency      = "Si.FORTS.1", # could specify virtual_id of the tracking currency
  #roll_pattern  = list(list(4, 10)),
  min_active_contract = 3,
  active_contract = 3
)

params <- bind_rows(params, data_frame(
  # limits
  
  limit.buy     = NA,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = NA,    # FIXME: no buy lower than 18
  risk.buy      = NA,
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = NA,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  
  symbol        = "Si.FORTS",   # exante prefix of contract series
  weight  = 0,
  power = 1,            # for power <0, bid = ask^power, ask=bid^power, for power>0 bid= bid^power, ask= ask^power
  currency       = NA,   # multiply by the midprice of the currency
  #roll_pattern  = list(list(4, 10)),
  min_active_contract = 1,
  active_contract = 1
))

r <- params %>% backtest("gamma", start=start, stop=stop, config=cfg) 
bt_reports(r, no_commission=T, currency="USD/RUB.MOEX", currency_power = -1)
#bt_view_metrics(r, start="2015-09-23 15:48:00", stop="2015-09-25")
#r$metrics %>% filter(pos!=lag(pos) | pos!=lead(pos)|bid!=lag(bid)|ask!=lag(ask)|bid!=lead(bid)|ask!=lead(ask)) %>% filter_date("2016-01-01","2016-05-01") %>% View()
bt_plot(r,no_gaps=F) # PLOT IN USD
#bt_plot(r,what="metrics.original", no_gaps=F) # PLOT IN THE ORIGINAL CURRENCY

#r$metrics %>% filter(!is.na(rtn)) %>% mutate(cumrtn = cumsum(rtn)) %>% plot_bt(metrics=c("price","pnl","pos","drisk","cumrtn"))
#print(bt_summaries(r))

#r$metrics %>% filter_date("2015-12-18") %>% filter(bid!=lead(bid)|ask!=lead(ask)|pos!=lead(pos)) %>% View()
