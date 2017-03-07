library(rticks)
library(ggplot2)

instrument_id <- "VIX.CBOE"
algo <- "gamma"

start <- dt(2016)

active_contract <- 1

no_cache <- F
no_clean <- F

config <- list(log_level=0)

params <- list(
  buy=c(17),
  sell=c(25),
  pos=c(0),
  mpi=c(0.05),
  spread=c(0.25),
  gamma.buy=c(1),
  gamma.sell=c(1),
  symbol=c("VIX.CBOE.1")
)

chunk <- query_candles_cache(instrument_id, start, active_contract) %>% fetch()

r <- chunk %>% backtest.chunk(params, algo=algo, config=config)

r %>% plot.backtest()



