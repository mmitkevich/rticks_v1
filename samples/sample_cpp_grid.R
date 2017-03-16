library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  roll_position = T # if T, then close position roll of ANY instrument (TODO: do it on real roll only). if F - roll position into next contract
  
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2015-01-01")
stop  <- as_datetime("2017-03-03")

params <- data_frame(
  # limits
  limit.buy     = 20.5,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 20,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19

  pos           = 0,     # initial position
  
  spread        = 0.25,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)

  symbol        = "VIX.CBOE",   # exante prefix of contract series

  active_contract = 6             # which month to trade
)

perfs <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)
results <- attr(perfs,"params")

perfs <- perfs %>% metrics.gamma(results) # calculate additional metrics

print(tail(perfs %>% spread(metric,value))) # print some perfs

perfs %>% plot_bt()

dir.create("~/rticks_bt", showWarnings=F)

write.csv(perfs %>% spread(metric, value), file=paste0("~/rticks_bt/",params$symbol))
