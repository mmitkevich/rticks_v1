#' backtest chunk
#' 
#' @examples 
#' query_candles_cache("VIX.CBOE", active_contract=1, start=dt(2016)) %>% fetch() %>% bt.chunk(params=list(...))
#' 
#' @export
backtest.chunk <- function(data, params, algo="gamma", config=list(freq="days")) {
  cat("backtest.chunk", as.character(data$datetime[1]),"\n")
  r <- bt_gamma(algo, data, params, config)
  d <- data %>% select(datetime, virtual_id, bid, ask) %>% as_data_frame()
  browser()
  d <- d %>% transmute(datetime=datetime, symbol=virtual_id, value=0.5*(bid+ask))
  d <- d %>% arrange(symbol, datetime) %>% as_data_frame()
  d$datetime <- d$datetime %>% trunc(config$freq) %>% as_date()
  d <- d %>% group_by(symbol, datetime) %>% filter(row_number()==n()) %>% mutate(metric="_price")
  r$perfs$datetime <- as_datetime(r$perfs$datetime) %>% trunc(config$freq) %>% as_date()
  r$perfs <- as_data_frame(r$perfs)
  r$perfs <- r$perfs %>% bind_rows(d) %>% arrange(datetime)
  return(r)
}

#' plot backtest results
#' 
#' @export
plot.backtest <- function(r, metrics=c("_price","pnl","rpnl","pos")) {
  df <- r$perfs %>%filter(metric %in% metrics)
  ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
    geom_line() + 
    facet_grid(metric ~ ., scales = "free_y")
} 
