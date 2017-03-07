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
  pos=c(0),
  mpi=c(0.05),
  spread=c(0.25),
  gamma.buy=c(1),
  gamma.sell=c(1),
  symbol=c("VIX.CBOE.1")
)

data.resample(data, freq="days") {
  data <- data %>% 
    transmute(
      datetime=as_date(trunc(data$datetime, "days")), 
    ) %>% 
    arrange(datetime) %>%
    group_by(datetime, symbol) %>%
    filter(row_number()==n()) %>% 
    as_data_frame()
  
}

bt <- function(algo, data, params, config=list()) {
  r <- bt_gamma("gamma", data, params, config)
  d <- data %>% select(datetime, virtual_id, bid, ask) %>% as_data_frame()
  d <- d %>% transmute(datetime=datetime, symbol=virtual_id, value=0.5*(bid+ask))
  d <- d %>% arrange(symbol, datetime) %>% as_data_frame()
  d$datetime <- d$datetime %>% trunc("days") %>% as_date()
  d <- d %>% group_by(symbol, datetime) %>% filter(row_number()==n()) %>% mutate(metric="_price")
  r$perfs$datetime <- as_datetime(r$perfs$datetime) %>% trunc("days") %>% as_date()
  r$perfs <- as_data_frame(r$perfs)
  #browser()
  r$perfs <- r$perfs%>%bind_rows(d)%>%arrange(datetime)
  return(r)
}

#data.resample(data, "days")
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

plot.bt <- function(r, metrics=c("price","pnl","rpnl","pos")) {
  df <- r$perfs %>%filter(metric %in% metrics)
  ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
    geom_line() + 
    facet_grid(metric ~ ., scales = "free_y")
} 
algo<-"gamma"
config <- list(log_level=0, warn_qty=10)
#r <- bt_gamma("gamma", data, params, config)
#r$perfs <- as_data_frame(r$perfs) %>%mutate(datetime=as_datetime(datetime))
#perfs%>%head
#r <- bt("gamma", data, params, config)
#plot.bt(r)
#print(r$pnl);
cat("DONE\n")

