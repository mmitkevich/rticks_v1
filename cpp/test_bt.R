library(rticks)
library(ggplot2)

options(debug=T)

myconfig = list(
  backtest = list(
    log_level=LOG$INFO,
    log_stdout=LOG$WARN,
    log_path="rticks.log",
    freq="days",
    no_cache = F,
    no_clean = F,
    no_save = F,
    check_big_qty = 3,
    roll_position = function(pos) 0
  ))

init_spd_log(myconfig$backtest)

algo <- "gamma"

# 2016 whole year

# single chunk
start<-as_datetime("2015-01-01")
stop<-as_datetime("2017-03-17")

params <- data_frame(
  # limits
  buy           = 20.5,
  sell          = +Inf,
  # initial position
  pos           = 0,
  # take profit
  spread        = 0.25,
  # size to buy on each mpi
  gamma.buy     = 1,
  # size to sell on each mpi
  gamma.sell    = 1,
  # exante prefix
  symbol=         "VIX.CBOE",
  # which month
  active_contract = 6
)

perfs <- params %>% backtest(algo, start=start, stop=stop, config=myconfig$backtest)

res <- attr(perfs,"params")

perfs <- gamma_metrics(perfs)

print(tail(perfs %>% spread(metric,value)))

perfs %>% plot_bt()

plot_price_pos <- function(perfs) {
  perfs %>% spread(metric,value) %>% select(pos, price) %>% ggplot(aes(x=price,y=pos)) + geom_point()
}

#perfs %>% plot_price_pos()