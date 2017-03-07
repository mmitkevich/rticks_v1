library(rticks)
library(ggplot2)

options(debug=T)

instrument_id <- "VIX.CBOE"
algo <- "gamma"

start <- dt(2016)

active_contract <- 1

no_cache <- F
no_clean <- F

config <- list(log_level=0)

params <- list(
  buy=c(1),
  sell=c(30),
  pos=c(0),
  mpi=c(0.05),
  spread=c(0.25),
  gamma.buy=c(1),
  gamma.sell=c(1),
  symbol=c("VIX.CBOE.1")
)

instrums  <- query_instruments(instrument_id)
data <- query_candles_cache(instrums, start, active_contract)

r <- data %>% backtest(params, "gamma", config)

print(r$pnl%>%head())

r$perfs %>% plot_bt()



