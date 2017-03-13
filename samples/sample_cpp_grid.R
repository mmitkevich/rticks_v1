library(rticks)
library(ggplot2)
library(grid)

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
    roll_position = T
  ))

init_spd_log(myconfig$backtest)

algo <- "gamma"

# 2016 whole year

# single chunk
start<-as_datetime("2014-01-01")
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

#perfs %>% plot_bt()

plot_price_pos <- function(perfs) {
  perfs %>% spread(metric,value) %>% select(pos, price) %>% ggplot(aes(x=price,y=pos)) + geom_point()
}


plot_bt1 <- function(perfs, metrics=c("price","pnl","rpnl","pos")) {
  df <- perfs %>% spread(metric,value) %>% arrange(datetime) %>%  filter(!is.na(price))

  pnl <- ggplotGrob(ggplot(df, aes(x=datetime, y=pnl, colour=symbol)) + geom_line())
#    theme_minimal() +
#    theme(axis.title.x = element_blank()))
  
  rpnl <- ggplotGrob(ggplot(df, aes(x=datetime, y=rpnl, colour=symbol)) + geom_line())
#    theme_minimal() +
#    theme(axis.title.x = element_blank()))
  
  grid.newpage()
  grid.draw(rbind(pnl, rpnl,size="last"))
}

perfs %>% plot_bt()

#perfs %>% plot_price_pos()