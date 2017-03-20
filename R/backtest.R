#' to frequency
#' 
#' @examples
#'  to_freq() 
#'  
#' @export
trunc_freq <- function(dts,freq) {
  trunc(dts/freq)*freq %>% as_datetime()
}

#' backtest chunk
#' 
#' @examples 
#' query_candles_cache("VIX.CBOE", active_contract=1, start=dt(2016)) %>% fetch() %>% bt.chunk(params=list(...))
#' 
#' @export
backtest.chunk <- function(data, params, algo, config) {
  log_perfs("backtest.chunk in", data, params, params, 0.5*(head(data$bid,1)+head(data$ask,1)))

  r <- bt_gamma(algo, data, params, config)
  
  if(length(r$perfs$datetime)==0) {
    wlog("backtest.chunk out empty perfs!", nrow(r$perfs$datetime), "rows", data$datetime %>% head(1) %>% as_datetime() %>% strftime("%y-%m-%d %H:%M:%S"),
         "..",data$datetime %>% tail(1) %>% as_datetime()%>% strftime("%y-%m-%d %H:%M:%S"))
    warning(paste("**** EMPTY PERFS ****",as.character(data$datetime[1]),"..",as.character(data$datetime%>%tail(1))))
    return (r);
  }
  
  d <- data %>% select(datetime, virtual_id, bid, ask) %>% as_data_frame()
  d <- d %>% transmute(datetime=datetime, symbol=virtual_id, price=0.5*(bid+ask))
  #d <- d %>% arrange(symbol, datetime) %>% as_data_frame()
  d$datetime <- d$datetime %>% trunc_freq(config$perfs_freq)
  browser()
  
  d <- d %>% group_by(symbol, datetime) %>% filter(row_number()==n())  %>% as_data_frame()
  browser()
  r$perfs$datetime <-  r$perfs$datetime %>% trunc_freq(config$perfs_freq)
  r$perfs <- as_data_frame(r$perfs)
  r$perfs <- r$perfs %>% spread(metric, value) %>% 
    inner_join(d, by=c("datetime","symbol")) %>%
    gather(metric, value, -datetime, -symbol) %>% 
    arrange(datetime)
  r$perfs$datetime <- as_date(r$perfs$datetime)
  log_perfs("backtest.chunk out",data, r, params, 0.5*(tail(data$bid,1)+tail(data$ask,1)))  
  return(r)
}

log_perfs <- function(name, data, r, params, price) {
  wlog(name, data$datetime %>% head(1) %>% as_datetime() %>% strftime("%y-%m-%d %H:%M:%S"),
       "..",data$datetime %>% tail(1) %>% as_datetime()%>% strftime("%y-%m-%d %H:%M:%S"),
       "nrow", nrow(data),
       "price", price,
       "pos", r$pos,
       "cash", r$cash,
       "qty_buy", r$qty_buy,
       "qty_sell", r$qty_sell,
       "pnl",r$pos*price*params$multiplier+r$cash,
       "exante_id", head(data$exante_id,1),
       "virtual_id", head(data$virtual_id,1))
}
#' backtest list of chunks
#' 
#' @export
backtest <- function(params, algo, start=NULL, stop=lubridate::now(), instruments=NULL, data=NULL, config=list(perfs_freq=days(1), no_cache=T, no_clean=T, no_save=T, custom_roll=NULL)) {
  if(is.null(instruments)) {
    instruments <- params$symbol
  }

  ilog("backtest","start",as.character(start),"stop",as.character(stop), "logging into ",config$log_path)

  instruments <- instruments %>% query_instruments()
  
  params <- as_data_frame(params) %>% 
    left_join(
      instruments %>% transmute(symbol=instrument_id, mpi=mpi, multiplier=multiplier), by="symbol") %>% 
    mutate(symbol=paste0(symbol,".",as.character(active_contract)))
  
  if(is.null(data)) {
    schedule <- load_trade_schedule(instruments$instrument_id, start = start, end=stop, exclude = FALSE)
    data <- instruments %>% query_candles_cache(active_contract=unique(params$active_contract), 
                                                roll_pattern=params$roll_pattern[1],
                                                start=start, stop=stop, 
                                                schedule=schedule,
                                                config=config)
                                                
  }
  
  perfs <- NULL
  params <- params %>% mutate(pos=0, cash=0, qty_buy=0, qty_sell=0)
  ilog("backtest algo", algo)
  for(chunk in data) {
    # open positions in the chunk
    if(nrow(chunk)==0) {
      wlog("backtest empty chunk "); #, as.character(as_datetime(attr(chunk,"start"))), as.character(as_datetime(attr(chunk,"stop"))))
    }else {
      ch = head(chunk,1)
      params$cash <- params$cash - params$pos*0.5*(ch$bid+ch$ask)*params$multiplier # open the pos
      r <- chunk %>% backtest.chunk(params, algo=algo, config=config)
      perfs <- perfs %>% bind_rows(r$perfs)
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
plot_bt <- function(perfs, start=NULL, stop=NULL, metrics=c("price","pnl","rpnl","pos")) {
  df <- perfs %>%filter(metric %in% metrics)
  df <- ifnull(start, df, df%>%filter(datetime>=start))
  df <- ifnull(stop, df, df%>%filter(datetime<stop))
  ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
    geom_line() + 
    facet_grid(metric ~ ., scales = "free_y")  + 
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m-%d") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
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
