#' to frequency
#' 
#' @examples
#'  to_freq() 
#'  
#' @export
trunc_freq <- function(dts,freq) {
  v <- as.numeric(dts)/as.numeric(freq)
  vv <- trunc(v)*as.numeric(freq) 
  vvv <- as_datetime(vv)
  vvv
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
  d <- d %>% transmute(datetime=datetime, symbol=virtual_id, bid=bid, ask=ask)
  #d <- d %>% arrange(symbol, datetime) %>% as_data_frame()
  d$datetime <- d$datetime %>% trunc_freq(config$perfs_freq)
  #browser()
  d <- d %>% group_by(symbol, datetime) %>% summarise(
    price = tail(0.5*(bid+ask),1),
    price_high = max(bid),
    price_low = min(ask)
  ) %>% as_data_frame()
  #browser()
  r$perfs$datetime <-  r$perfs$datetime %>% trunc_freq(config$perfs_freq)
  r$perfs <- as_data_frame(r$perfs)
  r$perfs <- r$perfs %>% spread(metric, value) %>% 
    inner_join(d, by=c("datetime","symbol")) %>%
    gather(metric, value, -datetime, -symbol) %>% 
    arrange(datetime)
  r$perfs$datetime <- as_datetime(r$perfs$datetime)
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

#' backtest default config
#' 
#' @export
backtest_config_default = list(
  perfs_freq=days(1), 
  
  no_cache=T, 
  no_clean=F, 
  no_save=T, 
  
  roll_position=T,
  custom_roll=NULL,
  
  log_path = "rticks.log",
  log_level = 3, #LOG$WARN,
  log_stdout = 3#LOG$WARN
)

#' backtest list of chunks
#' 
#' @export
backtest <- function(params, algo, start=NULL, stop=lubridate::now(), instruments=NULL, data=NULL, config=backtest_config_default) {
  
  config <- backtest_config_default %>% modifyList(config) # merge with default config
  config$perfs_freq <- as.numeric(config$perfs_freq)
  
  if(is.null(instruments)) {
    instruments <- params$symbol
  }

  ilog("backtest","start",as.character(start),"stop",as.character(stop), "logging into ",config$log_path)
  
  instruments <- instruments %>% query_instruments()
  if(nrow(instruments)==0) {
    wlog("instruments empty")
    stop("instruments empty")
  }
  
  cat("ORIGINAL PARAMS\n")
  print(params)
  if(is.null(data)) {
    schedule <- load_trade_schedule(instruments$instrument_id, start = start, end=stop, exclude = FALSE)
    q <- instruments %>% query_candles_cache(active_contract=unique(params$active_contract), 
                                                roll_pattern=params$roll_pattern[1],
                                                start=start, 
                                                stop=stop, 
                                                schedule=schedule,
                                                config=config)
    data <- q$data
    
  }
  
  if(nrow(params) > 1) {
    sp <- T
    
    weights.spread <- params$weight %>% setNames(paste0(params$symbol,".", params$active_contract))
    
    params <- as_data_frame(params) %>% 
      left_join(
        instruments %>% transmute(symbol=instrument_id, mpi=mpi, multiplier=multiplier), by="symbol")
    
    params <- data_frame(limit.buy = params$limit.buy[1],
                         stop.buy = params$stop.buy[1],
                         limit.sell = params$limit.sell[1],
                         stop.sell = params$stop.sell[1],
                         spread = params$spread[1], 
                         gamma.buy = params$gamma.buy[1],
                         gamma.sell = params$gamma.sell[1],
                         symbol = paste0(params$symbol[1], ".", params$active_contract[1],"-", params$symbol[2], ".", params$active_contract[2]),
                         weight = NA,
                         roll_pattern = NA,
                         active_contract = NA,
                         mpi = min(params$mpi),
                         multiplier = max(params$multiplier),
                         pos = 0,
                         cash = 0,
                         qty_buy = 0, 
                         qty_sell = 0)
  } else {
    sp <- F
    params <- as_data_frame(params) %>% 
      left_join(
        instruments %>% transmute(symbol=instrument_id, mpi=mpi, multiplier=multiplier), by="symbol") %>% 
      mutate(symbol=paste0(symbol,".",as.character(active_contract)))
  }
  perfs <- NULL
  params <- params %>% mutate(pos=0, cash=0, qty_buy=0, qty_sell=0)
  ilog("backtest algo", algo)
  
  cat("SPREAD PARAMS\n")
  print(params)
  #browser()
  active_contract_current <- 3
  for(chunk in data) {
    # open positions in the chunk
    if(nrow(chunk)==0) {
      wlog("backtest empty chunk "); #, as.character(as_datetime(attr(chunk,"start"))), as.character(as_datetime(attr(chunk,"stop"))))
    } else {
      ch = head(chunk,1)
      params$cash <- params$cash - params$pos*0.5*(ch$bid+ch$ask)*params$multiplier # open the pos
      #browser()
      
      if (sp == TRUE) {
        chunk <- chunk %>% synthetic.chunk(weights=weights.spread)
      }
      #browser
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
  perfs$datetime <- as_datetime(perfs$datetime)
  q$perfs<-perfs
  q$data<-data
  q$params<-params
  q
}

#' plot backtest results
#' 
#' @export
plot_bt <- function(perfs, start=NULL, stop=NULL, metrics=c("price","pnl","rpnl","pos")) {
  df <- data_frame()
  for(m in metrics) {
    ml = paste(m, "low", sep="_")
    mh = paste(m, "high", sep="_")
    d <- perfs %>%filter(metric==m | metric==mh | metric==ml) %>% 
      spread(metric, value) %>% mutate(metric=m) %>% rename_(.dots=list(close=m))
    if(has_name(d, ml))
      d <- d%>%rename_(.dots=list(low=ml))
    else
      d <- d%>%mutate(low=close)
    if(has_name(d, mh)) {
      d <- d%>%rename_(.dots=list(high=mh))
    }
    else
      d <- d%>%mutate(high=close)
    df <- bind_rows(df, d)
  }
  df <- ifnull(start, df, df%>%filter(datetime>=start))
  df <- ifnull(stop, df, df%>%filter(datetime<stop))
  timeframe <- sort(unique(df$datetime))
  timeframe <- timeframe[2]-timeframe[1]
  #ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
  #  geom_line() + 
  #  geom_linerange(aes(ymin=low, ymax=high)) +
  ggplot(df, aes(x=datetime,y=close,colour=symbol)) + theme_bw() +
    geom_segment(aes(y=close,yend=close, xend=datetime+0.5*timeframe)) + 
    geom_linerange(aes(ymin=low,ymax=high)) +
    facet_grid(metric ~ ., scales = "free_y")  + 
    scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m-%d") +
    scale_size_manual(values=0.5) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  #ggplot(d, aes(x=datetime)) + geom_segment(aes(y=close,yend=close, xend=datetime+1))+geom_linerange(aes(ymin=low,ymax=high))
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
