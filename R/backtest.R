#' backtest chunk
#' 
#' @examples 
#' query_candles_cache("VIX.CBOE", active_contract=1, start=dt(2016)) %>% fetch() %>% bt.chunk(params=list(...))
#' 
#' @export
backtest.chunk <- function(data, params, algo, config) {
  price <- 0.5*(head(data$bid,1)+head(data$ask,1))
  end_price <- 0.5*(tail(data$bid,1)+tail(data$ask,1))
  wlog("backtest.chunk >", data$datetime %>% head(1) %>% as_datetime() %>% strftime("%y-%m-%d %H:%M:%S"),
       "..",data$datetime %>% tail(1) %>% as_datetime()%>% strftime("%y-%m-%d %H:%M:%S"),
       "price",price,
       "pos",params$pos,
       "cash",params$cash,
       "equity",params$pos*price*params$multiplier+params$cash)
  #print(as.data.frame(config))
  #browser()
  r <- bt_gamma(algo, data, params, config)
  #browser()
  if(length(r$perfs$datetime)==0) {
    wlog("backtest.chunk < empty results")
    warning(paste("empty results",as.character(data$datetime[1]),"..",as.character(data$datetime%>%tail(1))))
    return (r);
  }
  #browser()
  
  d <- data %>% select(datetime, virtual_id, bid, ask) %>% as_data_frame()
  #browser()
  d <- d %>% transmute(datetime=datetime, symbol=virtual_id, price=0.5*(bid+ask))
  d <- d %>% arrange(symbol, datetime) %>% as_data_frame()
  d$datetime <- as_datetime(d$datetime) %>% trunc(config$freq) %>% as_date()
  d <- d %>% group_by(symbol, datetime) %>% filter(row_number()==n()) # %>% mutate(metric="price") #%>% filter(datetime>=min(r$datetime) & datetime<=max(r$datetime))
  r$perfs$datetime <- as_datetime(r$perfs$datetime) %>% trunc(config$freq) %>% as_date()
  r$perfs <- as_data_frame(r$perfs)
  #browser()
  r$perfs <- r$perfs %>% spread(metric, value) %>% 
    inner_join(d, by=c("datetime","symbol")) %>%
    gather(metric, value, -datetime, -symbol) %>% 
    arrange(datetime)
  r$perfs$datetime <- as_date(r$perfs$datetime)
  
  end_price <- 0.5*(tail(data$bid,1)+tail(data$ask,1))
  wlog("backtest.chunk <", data$datetime %>% head(1) %>% as_datetime() %>% strftime("%y-%m-%d %H:%M:%S"),
       "..",data$datetime %>% tail(1) %>% as_datetime()%>% strftime("%y-%m-%d %H:%M:%S"),
       "end_price",end_price,
       "end_pos",r$pos,
       "cash",r$cash,
       "equity",r$pos*end_price*params$multiplier+r$cash)
  
  return(r)
}

#' backtest list of chunks
#' 
#' @export
backtest <- function(params, algo, start=NULL, stop=lubridate::now(), instruments=NULL, data=NULL, config=list(freq="days", no_cache=T, no_clean=T, no_save=T)) {
  if(is.null(instruments)) {
    instruments <- params$symbol
  }

  ilog("backtest","start",as.character(start),"stop",as.character(stop),"\n")  
  ilog("logging into ",config$log_path)
  #file.remove(config$log_path)
  
  instruments <- instruments %>% query_instruments()
  
  params <- as_data_frame(params) %>% 
    left_join(
      instruments %>% transmute(symbol=instrument_id, mpi=mpi, multiplier=multiplier), by="symbol") %>% 
    mutate(symbol=paste0(symbol,".",as.character(active_contract)))
  
  if(is.null(data)) {
    schedule <- load_trade_schedule(instruments$instrument_id, start = start, end=stop, exclude = FALSE)
    data <- instruments %>% query_candles_cache(active_contract=unique(params$active_contract), 
                                                start=start, stop=stop, 
                                                schedule=schedule,
                                                config=config)
                                                
  }
  
  perfs <- NULL
  params <- params %>% mutate(pos=0, cash=0, qty_buy=0, qty_sell=0)
  ilog("backtest algo", algo)
  for(chunk in data) {
    #dlog(0, as.yaml(params))
    # open positions in the chunk
    if(nrow(chunk)==0) {
      ilog("skipped empty chunk", as.character(chunk$start),"\n")
    }else {
      ch = head(chunk,1)
      params$cash <- params$cash - params$pos*0.5*(ch$bid+ch$ask)*params$multiplier # open the pos
      #browser()
      r <- chunk %>% backtest.chunk(params, algo=algo, config=config)
      perfs <- perfs %>% bind_rows(r$perfs)
      #browser()
      params$pos  <- r$pos
      params$cash <- r$cash
      params$qty_buy <- r$qty_buy
      params$qty_sell <- r$qty_sell
      ct = tail(chunk,1)
      params$cash <- params$cash + params$pos*0.5*(ct$bid+ct$ask)*params$multiplier # close the position
      params$pos <- ifelse(config$roll_position, params$pos, 0) # calc new pos
    }
  }
  flush_spd_log()
  attr(perfs, "data") <- data
  attr(perfs, "params") <- params
  perfs$datetime <- as_date(perfs$datetime)
  perfs
}

#' plot backtest results
#' 
#' @export
plot_bt <- function(perfs, metrics=c("price","pnl","rpnl","pos")) {
  df <- perfs %>%filter(metric %in% metrics)
  ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
    geom_line() + 
    facet_grid(metric ~ ., scales = "free_y")
} 

#' add metrics
#' 
#' @export
metrics.gamma <- function(perfs, params) {
  #params = attr(perfs, "params")
  
  qtys <- perfs %>% spread(metric, value) %>% as_data_frame()
  
  qtys <- qtys %>% inner_join(params %>% select(symbol, spread, multiplier), by="symbol")
  qtys <- qtys %>% mutate(rpnl=pmin(qty_buy, qty_sell)*spread*multiplier) %>% select(-spread,-multiplier)
  qtys<- qtys %>% gather(metric, value, -datetime, -symbol)
  attr(qtys, "params") <- params
  qtys
}
