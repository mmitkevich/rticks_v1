cat("--1\n")
library(rticks)
if(FALSE){
cat("--2\n")
q <- query_candles("VIX", start=dt(2016))
cat("--3\n")
data <- fetch(q)
cat("--4\n")
saveRDS(data,"~/rticks/tests/VIX.rds")
}else {
  data<-readRDS("~/rticks/tests/VIX.rds")
}
results <- bt_gamma("gamma",
  data, 
  params = list(buy=c(20),sell=c(25)),
  config = list())
cat("DONE\n")

