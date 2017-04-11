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
  
  timer <- Sys.time()
  r <- bt_gamma(algo, data, params, config)
  log_perf(timer, nrow(data), "data processing speed ")
  
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

#' log_perf
#' 
#' @export
log_perf <- function(timer, nrows, what="speed") {
  dur <- Sys.time()-timer
  wlog(what, round(nrows/as.numeric(dur)), "op/s, nrows=", nrows, ", time spent", dur)
}

#' make virtual id for multiple instruments treated as the spread
#' 
#' @export

to_virtual_id <- function(symbols) {
  #symbols %>% by_row(function(s) {
  #  paste0(s$symbol,".",s$active_contract,ifelse(s$min_active_contract==s$active_contract,"",paste0("-",s$min_active_contract)))
  #}, .to="virtual_id", .collate="cols")
  symbols %>% mutate(
    virtual_id = paste0(
      symbol,
      ".",
      active_contract, 
      ifelse( min_active_contract == active_contract, 
              "",
              paste0("_",min_active_contract))))
  
#  paste0(params$symbol[1], ".", params$active_contract[1],"-", params$symbol[2], ".", params$active_contract[2])
}

#' filter_time("2015-01","2015-02")
#' 
#' @export
filter_date <- function(.x, start=NULL, stop=NULL) {
  if(!is.null(start))
    .x<-.x %>% filter(datetime>=dt(start))
  if(!is.null(stop))
    .x<-.x %>% filter(datetime<dt(stop))
  .x
}

#' time_of_Day
#' 
#' @export
time_of_day <- function(.x) {
  duration(as.numeric(.x) %% 86400) 
}

#' filter_time
#' 
#' @export
filter_time <- function(.x) {
 .x %>% filter(time_of_day()) 
}

#' backtest list of chunks
#' 
#' @export
backtest <- function(params, algo, start=NULL, stop=lubridate::now(), instruments=NULL, data=NULL, config=backtest_config_default) {
  timer <- Sys.time()
  
  config <- backtest_config_default %>% modifyList(config) # merge with default config
  config$perfs_freq <- as.numeric(config$perfs_freq)
  if(!has_name(params,"min_active_contract"))
    params$min_active_contract <- params$active_contract
  
  if(is.null(instruments)) {
    instruments <- params$symbol
  }

  wlog("backtest","start",as.character(start),"stop",as.character(stop), "logging into ",config$log_path)
  
  instruments <- instruments %>% query_instruments()
  if(nrow(instruments)==0) {
    wlog("instruments empty")
    stop("instruments empty")
  }
  
  #cat("ORIGINAL PARAMS\n")
  #print(params)
  vids <- to_virtual_id(params)$virtual_id 
  #browser()
  if(is.null(data)) {
    schedule <- load_trade_schedule(instruments$instrument_id, start = start, end=stop, exclude = FALSE)
    q <- instruments %>% query_candles_cache(active_contract = params$active_contract %>% setNames(params$symbol), 
                                             min_active_contract =params$min_active_contract %>% setNames(params$symbol),
                                                roll_pattern=params$roll_pattern[1],
                                                start=start, 
                                                stop=stop, 
                                                schedule=schedule,
                                                config=config)
    data <- q$data
    
  }

  more_params <- instruments %>% transmute(symbol=instrument_id, mpi=mpi, multiplier=multiplier, commission=commission)
  if(nrow(params) > 1) {
    sp <- T
    weights.spread <- params$weight %>% setNames(vids) # paste0(params$symbol,".", params$active_contract)
    
    params <- as_data_frame(params) %>% left_join(more_params, by="symbol")
    
    params <- data_frame(limit.buy = params$limit.buy[1],
                         stop.buy = params$stop.buy[1],
                         limit.sell = params$limit.sell[1],
                         stop.sell = params$stop.sell[1],
                         spread = params$spread[1], 
                         gamma.buy = params$gamma.buy[1],
                         gamma.sell = params$gamma.sell[1],
                         symbol = vids%>% reduce(~ paste0(.x, "-", .y)),
                         weight = NA,
                         roll_pattern = NA,
                         active_contract = NA,
                         mpi = min(params$mpi),
                         multiplier = max(params$multiplier),
                         commission = sum(params$commission),
                         pos = 0,
                         cash = 0,
                         qty_buy = 0, 
                         qty_sell = 0)
  } else {
    sp <- F
    params <- as_data_frame(params) %>% 
      left_join(more_params, by="symbol")
    params <-  params %>% to_virtual_id() %>% mutate(symbol=virtual_id)
  }
  perfs <- NULL
  params <- params %>% mutate(pos=0, cash=0, qty_buy=0, qty_sell=0)
  #browser()
  nrows <- data %>% map_dbl(nrow) %>% sum()
  log_perf(timer, nrows, "data loading speed ")
  timer <- Sys.time()
  #browser()
  data <- data %>% map(function(d) (d %>% group_by(lubridate::month(datetime)) %>% by_slice(~ ., .labels=F))$.out) %>% 
    purrr::flatten() %>% purrr::sort_by(~ .$datetime[1])
  for(chunk in data) {
    # open positions in the chunk
    if(nrow(chunk)==0) {
      wlog("backtest empty chunk "); #, as.character(as_datetime(attr(chunk,"start"))), as.character(as_datetime(attr(chunk,"stop"))))
    } else {
      if (sp == TRUE) {
        chunk <- chunk %>% synthetic.chunk(weights=weights.spread)
      }
      #browser()
      # FIXME: we need virtual_id=LH.CME.3/5 instead
      #chunk$virtual_id <- params$virtual_id
      
      ch = head(chunk,1)
      params$cash <- params$cash - params$pos*0.5*(ch$bid+ch$ask)*params$multiplier # open the pos
      #browser()
      
      #browser
      r <- chunk %>% backtest.chunk(params, algo=algo, config=config)
      perfs <- perfs %>% bind_rows(r$perfs)
      params$pos  <- r$pos
      params$cash <- r$cash
      if(r$qty_buy<params$qty_buy) {
        #browser()
      }
      params$qty_buy <- r$qty_buy
      params$qty_sell <- r$qty_sell

      ct = tail(chunk,1)
      params$cash <- params$cash + params$pos*0.5*(ct$bid+ct$ask)*params$multiplier # close the position
      params$pos <- ifelse(config$roll_position, params$pos, 0) # calc new pos

    }
    
  }
  log_perf(timer, nrows, "average data processing speed")
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
  plt <- ggplot(df, aes(x=datetime,y=close,colour=symbol)) + theme_bw() + theme(legend.position = "none") +
    geom_segment(aes(y=close,yend=close, xend=datetime+0.5*timeframe)) + 
    geom_linerange(aes(ymin=low,ymax=high)) + guides(fill=FALSE) +
    facet_grid(metric ~ ., scales = "free_y")  + 
    scale_size_manual(values=0.5) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + ggtitle(paste.list(unique(perfs$symbol),sep=","))
  dur = max(df$datetime)-min(df$datetime)
  if(dur>=ddays(90)){
    plt <- plt + scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(28)) {
    plt <- plt + scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(7)) {
    plt <- plt + scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "4 hour", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(1)) {
    plt <- plt + scale_x_datetime(date_breaks = "4 hour", date_minor_breaks = "1 hour",  date_labels = "%Y-%m-%d %H:%M")
  }else if(dur>dminutes(15)) {
    plt <- plt + scale_x_datetime(date_breaks = "1 minute", date_labels = "%Y-%m-%d %H:%M")
  }else {
    plt <- plt + scale_x_datetime(date_breaks = "1 second", date_labels = "%Y-%m-%d %H:%M:%S")
  }
  plt
  #ggplot(d, aes(x=datetime)) + geom_segment(aes(y=close,yend=close, xend=datetime+1))+geom_linerange(aes(ymin=low,ymax=high))
} 



#' bt_report(r)
#' 
#' @export
bt_reports <- function(r, start=NULL, stop=NULL) {
  # view data
  symbol <- paste0(paste.list(r$params$symbol,"-"))

  metrics <- metrics.gamma(r) # calculate additional metrics
  r$metrics <- metrics %>% spread(metric,value)
  
  # plot pnl
  
  # save results
  dir.create("~/rticks_bt", showWarnings=F)
  dt <- now() %>% strftime("%Y-%m-%d_%H-%M-%S")
  fn <- paste0("~/rticks_bt/",symbol)
  write.csv(r$schedule, file=paste(fn, dt, "schedule.csv", sep="."))
  write.csv(r$metrics, file=paste(fn, dt, "csv", sep="."))
  
  bt_plot(r, start=start,stop=stop)
}

#' view metrics
#' 
#' @export
bt_view_metrics<-function(r, start=NULL, stop=NULL) {
  metrics <- r$metrics
  if(!is.null(start))
    metrics<-metrics %>% filter(datetime>=as_datetime(start))
  if(!is.null(stop))
    metrics<-metrics %>% filter(datetime<=as_datetime(stop))
  metrics %>% View()
}

#' bt_plot
#'
#' @export
bt_plot<-function(r, start=NULL, stop=NULL) {
  r$metrics %>% gather(metric,value,-datetime,-symbol) %>% plot_bt(start=start,stop=stop)
}