library(rticks)
library(ggplot2)
library(grid)

options(debug=T)

cfg <- config(backtest) %>% modifyList(list(
  log_level=LOG$OFF,
  log_stdout=LOG$WARN,
  log_flush_level=LOG$WARN,
  perfs_freq=as.numeric(hours(24)),
  check_big_qty=10,
  no_cache = T, # всегда из базы
  no_save = T, # не писать кэш на диск
  roll_position = F, # if T, then close position roll of ANY instrument (TODO: do it on real roll only). if F - roll position into next contract
  custom_roll = roll_day(day_of_month=1) # months_ahead=0 at 1st of the month, at least 1 month ahead of expiration  
))

# init logging, see rticks.log
init_spd_log(cfg)

# period of backtest
start <- as_datetime("2015-10-01")
stop  <- as_datetime("2016-05-01")

contracSpecMonth <- data.frame(Number = c( 1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12),
                               Letter = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                               stringsAsFactors = FALSE)

params <- data_frame(
  # limits
  limit.buy     = 36.8,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 30,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = NA,    # FIXME: no sell above 19
  
  pos           = 0,     # initial position
  
  spread        = 0.2,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  
  symbol        = "ZL.CBOT",   # exante prefix of contract series
  
  #roll_pattern  = list(list(7, 12)),
  
  active_contract = 1             # which month to trade
)

perfs <- params %>% backtest("gamma", start=start, stop=stop, config=cfg)

data.backtest <- attr(perfs, "data")
schedule.new <- do.call("rbind",lapply(data.backtest, function(arg) data.frame(startDates=as.Date(min(arg$datetime)), endDates=as.Date(max(arg$datetime)),unique(arg$exante_id))))
View(schedule.new)
VIX_1 <- filter(perfs, (metric == "pos") | (metric == "pnl")) %>% spread(metric, value)
write.csv(x = VIX_1, file=paste0("~/Data/", params$symbol), row.names = F)

results <- attr(perfs,"params")

perfs <- perfs %>% metrics.gamma(results) # calculate additional metrics


# view perfs 
# perfs %>% spread(metric,value) %>% View()

perfs %>% plot_bt()

dir.create("~/rticks_bt", showWarnings=F)
write.csv(perfs %>% spread(metric, value), file=paste0("~/rticks_bt/",params$symbol))