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
  
  #r$perfs$datetime <-  r$perfs$datetime %>% trunc_freq(config$perfs_freq)
  r$perfs <- as_data_frame(r$perfs)
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
  
  zero_position=NULL,
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

#' 
#' 
#' @export
ndf <- function(names, val=numeric()){
  seq(1,length(names)) %>% map(~ val) %>% setNames(names) %>% as_data_frame()
}

#' time_frame_index
#'
#' @export
time_frame_index <- function(datetime, freq) {
  if(as.numeric(freq)>=as.numeric(months(1)))
    trunc((month(datetime)+12*year(datetime))/trunc(as.numeric(freq)/as.numeric(months(1))))
  else
    trunc(as.numeric(datetime)/as.numeric(freq))
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
  if(config$zero_position_freq) {
    data0 <- data
    data <- data %>% map(function(d) (d %>% group_by(time_frame_index(datetime, config$zero_position_freq)) %>% by_slice(~ ., .labels=F))$.out) %>% 
      purrr::flatten() %>% purrr::sort_by(~ .$datetime[1])
  }
  gaps = data_frame()
  ct = NULL
  tf_index <- -1
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
      if(!is.null(ct)) {
        tf_index1 <- time_frame_index(ch$datetime,config$zero_position_freq)
        if(!is.null(config$zero_position_freq) && tf_index1 != tf_index) {
            params$pos <- 0
            tf_index <- tf_index1
            wlog("zero_position_freq ",as.character(as_datetime(ch$datetime)))
        }else if(config$zero_position_on_rolls) {
          params$pos <- 0
          wlog("zero_position_on_rolls ",as.character(as_datetime(ch$datetime)))
        }
      }else {
        tf_index <- time_frame_index(ch$datetime,config$zero_position_freq)
      }
      if(!is.na(params$limit.buy) && params$pos>0 && ch$bid>params$limit.buy+params$spread) {
        params$pos <- 0
        wlog("zero_long_position_outside_limits ", "bid=",ch$bid, "ask=",ch$ask)
      }
      if(!is.na(params$limit.sell) && params$pos<0 && params$ch$ask<params$limit.sell-params$spread) {
        params$pos <- 0
        wlog("zero_short_position_outside_limits ", "bid=",ch$bid, "ask=",ch$ask)
      }

      if(!is.null(ct)) {
        gap <- data_frame(datetime=ch$datetime, gap = 0.5*((ch$bid+ch$ask)-(ct$bid+ct$ask)))
        gaps <- bind_rows(gaps,gap)
      }
      params$cash <- params$cash - params$pos*0.5*(ch$bid+ch$ask)*params$multiplier # open the pos
      #browser()
      
      #browser
      r <- chunk %>% backtest.chunk(params, algo=algo, config=config)
      perfs <- perfs %>% bind_rows(r$perfs)
      params$pos  <- r$pos
      params$cash <- r$cash
      params$qty_buy <- r$qty_buy
      params$qty_sell <- r$qty_sell

      ct = tail(chunk,1)
      params$cash <- params$cash + params$pos*0.5*(ct$bid+ct$ask)*params$multiplier # close the position
    }
    
  }
  log_perf(timer, nrows, "average data processing speed")
  flush_spd_log()
  perfs$datetime <- as_datetime(perfs$datetime)
  q$perfs<-perfs
  q$gaps<-gaps
  q$data<-data
  q$params<-params
  q
}

#' 
#' 
#' @export
lower_timeframe <- function(timeframe, nrows, maxrows=200, 
                            timeframes = c(minutes(1), minutes(10), hours(1), 
                                           hours(4), days(1), weeks(1), months(1), years(1))) {
  timeframe <- as.numeric(timeframe)
  timeframe.old <- timeframe
  timeframes <- as.numeric(timeframes)
  while(nrows*timeframe.old/timeframe > maxrows) {
    idx<-which.max(timeframes > timeframe)
    if(timeframes[idx] <= timeframe)
      break
    #browser()
    timeframe<-timeframes[idx]
  }
  as.period(timeframe)
}

#' 
#' 
#' @export
scale_x_datetime_smart <- function(plt, dur) {
  if(dur>=dyears(3)) {
    plt + scale_x_datetime(date_breaks = "1 year", date_minor_breaks = "3 month", date_labels = "%Y-%m-%d")
  }else  if(dur>=dyears(1)) {
    plt + scale_x_datetime(date_breaks = "3 month", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(90)){
    plt + scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(28)) {
    plt + scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(7)) {
    plt + scale_x_datetime(date_breaks = "1 day", date_minor_breaks = "4 hour", date_labels = "%Y-%m-%d")
  }else if(dur>=ddays(1)) {
    plt + scale_x_datetime(date_breaks = "4 hour", date_minor_breaks = "1 hour",  date_labels = "%Y-%m-%d %H:%M")
  }else if(dur>dminutes(15)) {
    plt #+ scale_x_datetime(date_breaks = "1 minute", date_labels = "%Y-%m-%d %H:%M")
  }else {
    plt #+ scale_x_datetime(date_breaks = "1 second", date_labels = "%Y-%m-%d %H:%M:%S")
  }
}

#' plot backtest results
#' 
#' @export
plot_bt <- function(perfs, start=NULL, stop=NULL, metrics=c("price","pnl","rpnl","pos"), maxrows=400) {
  #browser()
  perfs <- perfs %>% arrange(datetime)
  perfs <- ifnull(start, perfs, perfs %>% filter(datetime>=start))
  perfs <- ifnull(stop, perfs, perfs %>% filter(datetime<stop))
  timeframe <- as.period(perfs$datetime[2]-perfs$datetime[1])
  timeframe.old <- timeframe
  timeframe <- lower_timeframe(timeframe, nrow(perfs), maxrows=maxrows)
  
  df <- data_frame()
  for(m in metrics) {
    ml = paste(m, "low", sep="_")
    mh = paste(m, "high", sep="_")
    d <- perfs %>% select_(.dots=c("datetime", "symbol", m, ml, mh) %>% intersect(names(perfs))) %>% 
      rename_(.dots=list(close=m))
    if(has_name(d, ml))
      d <- d %>% rename_(.dots=list(low=ml))
    else
      d <- d %>% mutate(low=close)
    if(has_name(d, mh))
      d <- d %>% rename_(.dots=list(high=mh))
    else
      d <- d %>% mutate(high=close)
    
    d1 <- d %>% ohlc(freq=timeframe)
    df <- bind_rows(df, d1 %>% mutate(metric=m))
  }
  df1 <- df %>% arrange(datetime) %>% as_data_frame()
  #browser()
  #ggplot(df, aes(x=datetime, y=value, colour=symbol)) + 
  #  geom_line() + 
  #  geom_linerange(aes(ymin=low, ymax=high)) +
  #browser()
  plt <- ggplot(df1, aes(x=datetime, y=close, colour=symbol)) + 
    theme_bw() + 
    theme(legend.position = "none") +
    geom_segment(aes(y=close, yend=close, xend=datetime+timeframe)) + 
    geom_linerange(aes(ymin=low, ymax=high)) + guides(fill=FALSE) +
    facet_grid(metric ~ ., scales = "free_y")  + 
    scale_size_manual(values=0.5) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    ggtitle(paste.list(unique(perfs$symbol),sep=","))
  #browser()
  scale_x_datetime_smart(plt, max(df$datetime)-min(df$datetime))
  #ggplot(d, aes(x=datetime)) + geom_segment(aes(y=close,yend=close, xend=datetime+1))+geom_linerange(aes(ymin=low,ymax=high))
} 



#' bt_report(r)
#' 
#' @export
bt_reports <- function(r, start=NULL, stop=NULL, save=F, ...) {
  # view data
  metrics <- metrics.gamma(r, ...) # calculate additional metrics
  r$metrics <- metrics %>% spread(metric,value)
  
  # plot pnl
  
  # save results
  if(save) {
    symbol <- paste0(paste.list(r$params$symbol,"-"))
    dir.create("~/rticks_bt", showWarnings=F)
    dt <- now() %>% strftime("%Y-%m-%d_%H-%M-%S")
    fn <- paste0("~/rticks_bt/",symbol)
    write.csv(r$schedule, file=paste(fn, dt, "schedule.csv", sep="."))
    write.csv(r$metrics, file=paste(fn, dt, "csv", sep="."))
  }
}

#' view metrics
#' 
#' @export
bt_view_metrics <- function(r, start=NULL, stop=NULL) {
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
bt_plot<-function(r, start=NULL, stop=NULL, maxpoints=400, no_gaps=F) {
  metrics <- r$metrics
  if(no_gaps) {
    metrics$chunk <- metrics$datetime %>% findInterval(r$gaps$datetime)+1
    for(i in seq(1,nrow(r$gaps)-1)) {
      metrics$price[metrics$chunk<=i] <- metrics$price[metrics$chunk<=i] + r$gaps$gap[i] 
      metrics$price_low[metrics$chunk<=i] <- metrics$price_low[metrics$chunk<=i] + r$gaps$gap[i] 
      metrics$price_high[metrics$chunk<=i] <- metrics$price_high[metrics$chunk<=i] + r$gaps$gap[i] 
    }
  }
  metrics %>% plot_bt(start=start,stop=stop,maxrows=maxpoints)
}

bt_summaries <- function(r, start=NULL, stop=NULL) {
  m <- r$metrics %>% filter_date(start,stop)
  df <- data_frame()
  for(x in list("buy","sell","pos","price")) {
    d <- c(m[[paste(x,"low",sep="_")]], m[[paste(x,"high",sep="_")]])
    d <- d[!is.na(d) & !is.infinite(d)]
    d <- d %>% summary() %>% as.list() %>% as_data_frame()
    d$metric = x
    df <- bind_rows(df, d)
  }
  df
}
