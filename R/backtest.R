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
backtest.chunk <- function(data, params, algo, config, signals) {
  log_perfs("backtest.chunk in", data, params, params, 0.5*(head(data$bid,1)+head(data$ask,1)))
  
  timer <- Sys.time()
  
  config$cookie <- paste0(params$symbol,".spread~",params$spread,".iis~",params$iis)
  r <- bt_gamma(algo, data, params, config, signals)
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
  
  zero_position_on_rolls=F,
  zero_position_freq=NULL,
  custom_roll=NULL,
  start_full_pos=F,
  
  roll_price="best",
  
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
backtest <- function(params, algo, stparams=NULL, start=NULL, stop=lubridate::now(), instruments=NULL, data=NULL, config=backtest_config_default, signals=list()) {
  timer <- Sys.time()
  
  config <- backtest_config_default %>% modifyList(config) # merge with default config
  config$perfs_freq <- as.numeric(config$perfs_freq)
  if(!has_name(params,"min_active_contract"))
    params$min_active_contract <- params$active_contract
  if(!has_name(params,"power"))
    params$power <- rep(1, nrow(params))
  if(!has_name(params,"currency"))
    params$currency <- rep(NA, nrow(params))
  if(!has_name(params,"bid"))
    params$bid <- rep(NA, nrow(params))
  if(!has_name(params,"ask"))
    params$ask <- rep(NA, nrow(params))
  if(!has_name(params,"pos"))
    params$pos <- rep(NA, nrow(params))
  if(!has_name(params,"weight")) {
    wlog("All weights to 1")
    params$weight <- seq(1, nrow(params))
  }
  
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
  q <- new.env() %>% structure(class="reuters")
  if(is.null(data)) {
    schedule <- load_trade_schedules(instruments, start = start, end=stop, exclude = FALSE)
    q <- instruments %>% query_candles_cache(active_contract = params$active_contract %>% setNames(params$symbol), 
                                             min_active_contract =params$min_active_contract %>% setNames(params$symbol),
                                                roll_pattern=params$roll_pattern[1],
                                                start=start, 
                                                stop=stop, 
                                                schedule=schedule,
                                                config=config)
    data <- q$data
    #browser()
  }

  more_params <- instruments %>% transmute(symbol=instrument_id, mpi=mpi, multiplier=multiplier, commission=commission)
  #if(nrow(params) > 1) {
    sp <- T
    weights.spread <- params$weight %>% setNames(vids) # paste0(params$symbol,".", params$active_contract)
    powers.spread <- params$power %>% setNames(vids)
    currency.spread <- params$currency %>% setNames(vids)
    
    params <- as_data_frame(params) %>% left_join(more_params, by="symbol")
    
    params <- params[1,] %>%  mutate(symbol = vids%>% reduce(~ paste0(.x, "-", .y)),
                         weight = NA,
                         roll_pattern = NA,
                         active_contract = NA,
                         mpi = min(params$mpi),
                         multiplier = max(params$multiplier),
                         commission = sum(params$commission))
    if(!is.null(config$multiplier))
      params$multiplier <- config$multiplier
    if(!is.null(config$mpi))
      params$mpi <- config$mpi
    
  
  nrows <- data %>% map_dbl(nrow) %>% sum()
  if(!is.null(config$zero_position_freq)) {
    data0 <- data
    data <- data %>% keep(~ nrow(.)>0) %>% map(function(d) (d %>% group_by(time_frame_index(datetime, config$zero_position_freq)) %>% by_slice(~ ., .labels=F))$.out) %>% 
      purrr::flatten() %>% purrr::sort_by(~ .$datetime[1])
  }
  log_perf(timer, nrows, "data loading speed ")
  log_path <- config$log_path
  istpars <- seq(1, nrow(stparams))
  runs <-  istpars %>% parmap( function(istpar) {
  #for(istpar in seq(1,nrow(stparams)))
  #runs <- seq(1, nrow(stparams)) %>% map(function(istpar) {  
    #cfg<-config
    #cfg$log_path <- paste0(log_path,"-",Sys.getpid())
    #wlog("Logging into",cfg$log_path)
    #init_spd_log(cfg)
    
    timer <- Sys.time()
    
    price.old<-NA
    cash.old<-NA
    perfs <- NULL
    params <- params %>% mutate(cash=0, qty_buy=0, qty_sell=0, bid=NA, ask=NA)
    
    for(op in names(stparams[istpar,])) {
      params[op] <- stparams[istpar,op]
    }
    
    wlog("USING strategy parameters")
    wlog(paste0("\n",df_chr(stparams[istpar,])))
    
    gaps = data_frame()
    ct = NULL
    tf_index <- -1
    data.spread<-list()
    is_roll<-F
    price.old <- NA
    # data loaded into memory. So can use a lot of params here
    for(chunk in data) {
      # open positions in the chunk
      #if (sp == TRUE && nrow(chunk)>0) {
      #}
      if(nrow(chunk)==0) {
        wlog("backtest empty chunk "); #, as.character(as_datetime(attr(chunk,"start"))), as.character(as_datetime(attr(chunk,"stop"))))
      } else {
        chunk <- chunk %>% synthetic.chunk(weights=weights.spread, powers=powers.spread, currencies=currency.spread, mpi=config$mpi)
        data.spread <- c(data.spread,list(chunk))
        ch = head(chunk,1)
        is_roll <- !is.null(ct) && ch$exante_id!=ct$exante_id
        if(!is_null(ct)) {
          price.old <- ifelse(config$roll_price=="mid" || !is_roll, 0.5*(ct$ask+ct$bid), ifelse(params$pos>0, ct$bid, ct$ask))
          params$cash <- params$cash + params$pos*price.old*params$multiplier # close the position
        }
        if(is.na(params$pos)||config$start_full_pos) { # first
          dpos <- (max(params$limit.buy-ch$ask,0)/params$mpi)*params$gamma.buy - na_replace(params$pos)
          wlog("START delta(pos) = delta(qty_buy/qty_sell) = ", dpos)
          #browser()
          params$pos <- na_replace(params$pos) + dpos
          #if(dpos>0)
          #  params$qty_buy <- params$qty_buy + dpos
          #else
          #  params$qty_sell <- params$qty_sell - dpos
          params$gamma_pos.buy <- params$kpos/((params$limit.buy-params$stop.buy)/params$mpi) # maximum position
          #params$gamma_pos.sell <- params$kpos/((params$limit.buy-params$stop.buy)/params$mpi) # maximum position
          params$gamma_pos.sell <- 0
        }
  
        # FIXME: we need virtual_id=LH.CME.3/5 instead
        #chunk$virtual_id <- params$virtual_id
        if(!is.null(config$zero_position_freq))
          if(!is.null(ct)) {
            tf_index1 <- time_frame_index(ch$datetime,config$zero_position_freq)
            if(!is.null(config$zero_position_freq) && tf_index1 != tf_index) {
                params$pos <- 0
                tf_index <- tf_index1
                wlog("ZERO POS (MONTHLY) ",as.character(as_datetime(ch$datetime)))
            }else if(is_roll && config$zero_position_on_rolls) {
              params$pos <- 0
              wlog("ZERO POS (ROLL) ",as.character(as_datetime(ch$datetime)))
            }
          }else {
            tf_index <- time_frame_index(ch$datetime,config$zero_position_freq)
          }
        
        if(is_roll) {
          gap <- data_frame(datetime=ch$datetime, gap = ifelse(params$pos>=0,ch$bid-ct$bid, ch$ask-ct$ask), roll_from=ct$exante_id, roll_into=ch$exante_id)
          gaps <- bind_rows(gaps,gap)
        }else {
          gap <- data_frame(datetime=ch$datetime, gap = 0)
        }
        old.bid <- params$bid
        old.ask <- params$ask
        if(isTRUE(params$bid<params$limit.buy)) {
          params$bid <- params$bid+gap$gap
          params$ask <- params$ask+gap$gap
        }else if(isTRUE(params$bid==params$limit.buy)) {
          params$bid <- min(params$limit.buy, ch$bid)
          params$ask <- NA
        }
        
        pos.old <- params$pos      
        if(is_roll && !config$start_full_pos && !is.na(params$limit.buy) && !is.infinite(params$limit.buy) && isTRUE(params$pos>0)) {
          pos.max <- trunc(max(0,((params$limit.buy+params$spread-params$ask)/params$mpi+1.00001)*params$gamma.buy))
          if(isTRUE(params$pos>pos.max)) {
            params$pos <- pos.max
            
          }
        }
        if(is_roll && !config$start_full_pos && !is.na(params$limit.sell) && !is.infinite(params$limit.sell) && isTRUE(params$pos<0)) {
          pos.min <- trunc(min(0,((params$limit.sell-params$spread-params$bid)/params$mpi-1.0001)*params$gamma.sell))
          if(params$pos<pos.min) {
            params$pos <- pos.min
          }
        }
        price.new <- ifelse(config$roll_price=="mid" || !is_roll,  0.5*(ch$ask+ch$bid), ifelse(params$pos>0, ch$ask, ch$bid))
        if(is_roll) {
          wlog("ROLL", "from=", ct$bid,ct$ask,"into=",ch$bid, ch$ask, "gap=",gap$gap)      
          wlog("SPREAD", "old=",old.bid, old.ask, "new=",params$bid,params$ask)
          
          wlog("POS old=",pos.old, "new=",params$pos)
          wlog("PRICE old=",price.old, "new=",price.new)
        }
        params$cash <- params$cash - params$pos*price.new*params$multiplier # open the pos
        #browser()
        
        ct = tail(chunk,1)        
        signals.chunk <- signals %>% map(~ .x %>% filter(datetime>=ch$datetime & datetime<ct$datetime+60))
        r <- chunk %>% backtest.chunk(params, algo=algo, config=config, signals=signals.chunk)
        perfs <- perfs %>% bind_rows(r$perfs)
        params$pos  <- r$pos
        params$cash <- r$cash
        params$qty_buy <- r$qty_buy
        params$qty_sell <- r$qty_sell
        params$bid <- r$bid 
        params$ask <- r$ask
        if(!is.null(signals.chunk$spread) && nrow(signals.chunk$spread)>0) {
          params$spread <- tail(signals.chunk$spread$value, 1)
        }
      }
    }
    log_perf(timer, nrows, "average data processing speed")
    perfs$datetime <- as_datetime(perfs$datetime)
    flush_spd_log()
    qq <- new.env(); #as.environment(as.list(q, all.names=TRUE))
    for(n in ls(q, all.names=TRUE)) assign(n, get(n, q), qq)
    qq$perfs <- perfs
    qq$gaps <- gaps
    qq$config <- config
    qq$data <- data
    qq$data.spread <- data.spread
    qq$params <- params
    qq$stparams <- params
    qq
  })
  runs
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
plot_bt <- function(perfs, start=NULL, stop=NULL, enabled=c("price","pnl","rpnl","pos"), maxrows=400) {
  #browser()
  perfs <- perfs %>% arrange(datetime)
  perfs <- ifnull(start, perfs, perfs %>% filter(datetime>=start))
  perfs <- ifnull(stop, perfs, perfs %>% filter(datetime<stop))
  dt <- unique(perfs$datetime)
  timeframe <- as.period(dt[2]-dt[1])
  timeframe.old <- timeframe
  timeframe <- lower_timeframe(timeframe, nrow(perfs), maxrows=maxrows)
  
  df <- data_frame()
  for(m in enabled) {
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
    theme(legend.position = "bottom") +
    geom_segment(aes(y=close, yend=close, xend=datetime+timeframe)) + 
    geom_linerange(aes(ymin=low, ymax=high)) + guides(fill=FALSE) +
    facet_grid(metric ~ ., scales = "free_y")  + 
    scale_size_manual(values=0.5) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
    ggtitle(paste.list(unique(perfs$symbol),sep=","))
  #browser()
  scale_x_datetime_smart(plt, max(df$datetime)-min(df$datetime)) + scale_y_continuous(label=dollar_format(suffix="",prefix=""))
  #ggplot(d, aes(x=datetime)) + geom_segment(aes(y=close,yend=close, xend=datetime+1))+geom_linerange(aes(ymin=low,ymax=high))
} 



#' bt_report(r)
#' "USD/RUB.MOEX" to divide by the currency rate
#' @export
bt_reports <- function(r, start=NULL, stop=NULL, currency=NULL, currency_power=1, save=F, signals=NULL,...) {
  # view data
  r$metrics <- r$perfs %>% spread(metric, value) %>% as_data_frame()
  if(!is.null(signals))
    r$metrics <- r$metrics %>% 
      left_join(signals, by="datetime") %>% 
      fill_(names(signals) %>% setdiff("datetime"))
  r$metrics <- r %>% metrics.gamma(...) # calculate additional metrics
  r$metrics.original <- r$metrics
  if(!is.null(currency)) {
    if(!is.data.frame(currency)) {
      cur <- query_candles(currency, start=min(r$metrics$datetime), stop=max(r$metrics$datetime)) %>% fetch_all() %>% 
        reduce(bind_rows)
      cur <- cur %>% to_freq(r$config$perfs_freq, tz_offset=r$config$perfs_tz, by="datetime") %>% 
        as_data_frame() %>% transmute(datetime=datetime, cur_bid=bid, cur_ask=ask)
      r$currency <- cur
    }else{
      r$currency <- currency
    }
    
    r$metrics <- r$metrics %>% inner_join(cur, by="datetime") %>% mutate(
      cur = ifelse(pos>0, cur_bid, cur_ask)) %>% 
    filter(cur!=0 & cur_bid<=cur_ask) %>% mutate(
      commission = cur^currency_power*commission,
      assets = pnl-cash, assets_high=pnl_high-cash, assets_low=pnl_low-cash,
      cash = cumsum((cash-lag(cash,default=0))*cur^currency_power),
      #pnl = cur^currency_power*(pnl-cash)+cash, 
      pnl = cash + assets*cur^currency_power,
      pnl_high = cash + assets_high*cur^currency_power,
      pnl_low = cash + assets_low*cur^currency_power,
      drisk = cur^currency_power*drisk,
      rpnl = cumsum((rpnl-lag(rpnl,default=0))*cur^currency_power)
    ) %>% select(-cur, -cur_bid, -cur_ask, -assets, -assets_high, -assets_low)
  }
  
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
  r
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
bt_plot<-function(r, what="metrics", start=NULL, stop=NULL, enabled=c("price","pnl","rpnl","pos"), maxpoints=400, no_gaps=F) {
  metrics <- r[[what]]
  if(no_gaps) {
    metrics$chunk <- metrics$datetime %>% findInterval(r$gaps$datetime)+1
    for(i in seq(1,nrow(r$gaps)-1)) {
      metrics$price[metrics$chunk<=i] <- metrics$price[metrics$chunk<=i] + r$gaps$gap[i] 
      metrics$price_low[metrics$chunk<=i] <- metrics$price_low[metrics$chunk<=i] + r$gaps$gap[i] 
      metrics$price_high[metrics$chunk<=i] <- metrics$price_high[metrics$chunk<=i] + r$gaps$gap[i] 
    }
  }
  metrics %>% plot_bt(start=start,stop=stop,maxrows=maxpoints, enabled=enabled)
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
