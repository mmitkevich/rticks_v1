cat("--1\n")
library(rticks)
if(F){
cat("--2\n")
q <- query_candles("VIX", start=dt(2016), active_contract = 1)
cat("--3\n")
data <- fetch(q)
cat("--4\n")
saveRDS(data,"~/rticks/tests/VIX.rds")
}else {
  data0<-readRDS("~/rticks/tests/VIX.rds")
  schedule <- load_trade_schedule(instrument_id = "VIX.CBOE", start = dt(2016), exclude = FALSE)
  cut_minutes <- 3
  
  data<-clean.chunk(data0, 
              schedule, 
              cut_minutes, 
              negative_bidask = TRUE)
}

params <- list(
  buy=c(20),
  sell=c(25),
  mpi=c(0.05),
  spread=c(0.25),
  gamma.buy=c(1),
  gamma.sell=c(1),
  symbol=c("VIX.CBOE.1")
)
config <- list()
results <- bt_gamma("gamma", data, params, config)
print(results)
cat("DONE\n")

