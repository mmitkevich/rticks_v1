library(rticks)
library(ggplot2)

options(debug=T)

instruments <- "VIX.CBOE"
algo <- "gamma"

start <- dt(2016)

active_contract <- 1

no_cache <- F
no_clean <- F

params <- list(
  buy=c(20),
  sell=c(30),
  pos=c(0),
  mpi=c(0.05),
  spread=c(0.25),
  gamma.buy=c(1),
  gamma.sell=c(1),
  symbol=c("VIX.CBOE.1")
)

instruments  <- query_instruments(instruments)
data <- query_candles_cache(instruments, start, active_contract)

r <- data %>% backtest(params, "gamma", config = list(log_level=3, freq="days"))

print(r$pnl%>%head())

#r$perfs %>% plot_bt()



