#' backtest chunk
#' 
#' @examples 
#' query_candles_cache("VIX.CBOE", active_contract=1, start=dt(2016)) %>% fetch() %>% bt.chunk(params=list(...))
#' 
#' @export
backtest.chunk <- function(data, params, algo, config) {
  cat("backtest.chunk", as.character(data$datetime[1]),"..",as.character(data$datetime%>%tail(1)),"\n")
  #browser()
  r <- bt_gamma(algo, data, params, config)
  d <- data %>% select(datetime, virtual_id, bid, ask) %>% as_data_frame()
  #browser()
  d <- d %>% transmute(datetime=datetime, symbol=virtual_id, value=0.5*(bid+ask))
  d <- d %>% arrange(symbol, datetime) %>% as_data_frame()
  d$datetime <- d$datetime %>% trunc(config$freq) %>% as_date()
  d <- d %>% group_by(symbol, datetime) %>% filter(row_number()==n()) %>% mutate(metric="_price")
  r$perfs$datetime <- as_datetime(r$perfs$datetime) %>% trunc(config$freq) %>% as_date()
  r$perfs <- as_data_frame(r$perfs)
  r$perfs <- r$perfs %>% bind_rows(d) %>% arrange(datetime)
  return(r)
}

#' backtest list of chunks
#' 
#' @export
backtest <- function(params, algo, start=NULL, stop=lubridate::now(), instruments=NULL, data=NULL, config=list(freq="days", no_cache=T, no_clean=T, no_save=T)) {
  if(is.null(instruments)) {
    instruments <- params$symbol
  }
  
  instruments <- instruments %>% query_instruments()
  
  params <- as_data_frame(params) %>% left_join(instruments %>% transmute(symbol=instrument_id, mpi=mpi), by="symbol") %>% 
    mutate(symbol=paste0(symbol,".",as.character(active_contract)))
  
  if(is.null(data)) {
    schedule <- load_trade_schedule(instruments$instrument_id, start = start, end=stop, exclude = FALSE)
    data <- instruments %>% query_candles_cache(active_contract=unique(params$active_contract), 
                                                start=start, stop=stop, 
                                                schedule=schedule,
                                                config=config)
                                                
  }
  
  perfs <- NULL
  pnl <- NULL
  pos <- NULL
  rpnl <- NULL
  
  params = as_data_frame(params)
  params <- params %>% mutate(pnl=0, rpnl=0)
  
  cat("backtest algo", algo, "\nparams:\n")
  print(as.data.frame(params))
  for(chunk in data) {
    #print(params)
    r1 <- chunk %>% backtest.chunk(params, algo=algo, config=config)
    perfs <- bind_rows(perfs, r1$perfs)
    #browser()
    params$pos  <- r1$pos
    params$pnl  <- r1$pnl
    params$rpnl <- r1$rpnl
    pnl <- r1$pnl
    rpnl <-r1$rpnl
    pos <- r1$pos
  }
  attr(perfs, "data") <- data
  attr(perfs, "results") <- params %>% mutate(pnl=pnl, pos=pos, rpnl=rpnl)
  perfs
}

#' plot backtest results
#' 
#' @export
plot_bt <- function(perfs, metrics=c("_price","pnl","rpnl","pos")) {
  df <- perfs %>%filter(metric %in% metrics)
  ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
    geom_line() + 
    facet_grid(metric ~ ., scales = "free_y")
} 
