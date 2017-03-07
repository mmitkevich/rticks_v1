library(rticks)
library(ggplot2)

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
  buy=c(17),
  sell=c(25),
  pos=c(100),
  mpi=c(0.05),
  spread=c(0.25),
  gamma.buy=c(1),
  gamma.sell=c(1),
  symbol=c("VIX.CBOE.1")
)

bt <- function(data, algo, params, config) {
  raw <- bt_gamma(algo, data, params, config)
  metrics <- bt_metrics(raw)
  dates <- as_date(trunc(metrics$datetime,"days"))
  metrics <- metrics%>%mutate(datetime=dates)
  dates <- as_date(trunc(data$datetime,"days"))
  price <- data%>%transmute(datetime=dates, symbol=virtual_id, price=0.5*(bid+ask))%>%arrange(datetime)%>%group_by(datetime, symbol) %>% filter(row_number()==n()) %>% as_data_frame()
  #browser()
  df <- left_join(metrics, price, by=c("datetime","symbol"))
  return(df)
}

bt_metrics <- function(results, cols=c("pnl","rpnl", "pos")) {
  dates = as_datetime(results$datetime)
  symbols <- names(results$pnl)
  results$datetime<-NULL
  r <- results %>% map(~ as.data.frame(list(datetime=dates) %>% modifyList(.x)))
  r <- r %>% map2(names(r), ~ .x %>% gather_("symbol", .y, symbols))
  r <- r %>% reduce(~ inner_join(.x,.y,by=c("datetime","symbol")))
  r <- ifnull(cols, r, r %>% select_(.dots=c("datetime","symbol", cols)))
  r
}

plot.bt <- function(r) {
  r <- r %>% gather(metric,value,-datetime,-symbol)%>%arrange(datetime)
  ggplot(r, aes(x=datetime, y=value, colour=metric, group=metric)) + 
    geom_line() + 
    facet_grid(metric ~ ., scales = "free_y")
} 

config <- list(log_level=0, warn_qty=10)
#r <- bt("gamma", data, params, config)
#plot.bt(r)
#print(r$pnl);
cat("DONE\n")

