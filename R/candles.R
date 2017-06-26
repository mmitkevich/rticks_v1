#' query candles from candles provider
#' 
#' @export

query_candles <- function(instruments = NULL, 
                          schedule = NULL,
                          active_contract = 1,
                          min_active_contract = 1,
                          custom_roll = NULL,
                          start = NULL, 
                          stop = lubridate::now(), 
                          provider = "reuters", ...) {
  query_fn <- get(paste0("query_candles.", provider))
  query_fn(instruments = instruments, 
           schedule = schedule,
           active_contract = active_contract,
           min_active_contract = min_active_contract,
           custom_roll = custom_roll,
           start = start,
           stop = stop,...)
}

  
#' print chunk
#' 
#' @export
print.chunk <- function(q) {
  cat("start: ", as.character(as_datetime(q$start)),
      ", stop:", as.character(as_datetime(q$stop)), 
      ", symbols: ", (q$mapping%>%arrange(active_contract))$exante_id %>% paste,"\n")
}

#' fetch all chunks
#' 
#' @export
fetch_all <- function(q) {
  chunk <- fetch(q)
  result <- list()
  i <- 1
  while(!is.null(chunk)) {
    result[[i]] <- chunk
    i <- i+1
    chunk <- fetch(q)
  }
  return(result)
}

#' rbind chunks
#' 
#' @export
combine.chunks <- function(chunks) {
  setNames(nm=.reuters.fields) %>% map(function(f) {
    chunks %>% map( ~ .x[[f]] )  %>% bind_rows()
  })
}


#' clean chunks
#' 
#' @examples
#'  chunks %>% map(~clean.chunk(., time_filter=)
#' 
#' @export
clean_mim.chunk <- function(chunk, 
                        time_filter = NULL, 
                        value_filter = NULL) {
  time_filter = ifnull(time_filter, ~ map_lgl(., ~ T)) %>% as_function()
  value_filter = ifnull(value_filter, ~ map_lgl(., ~ .!=0 & !is.na(.))) %>% as_function
  for(f in chunk$fields) {
    df <- chunk[[f]]
    # apply value_filter to each instrument
    vf <- colnames(df) %>% 
      keep(~ .x != "datetime") %>% 
      map(~ df[[.x]]) %>% 
      map(function(x) value_filter(x)) %>% 
      reduce(`&`) 
    tf <- df$datetime %>% time_filter
    chunk[[f]] <- df %>% filter(vf & tf)
  }
  return(chunk)
}

#' query_candles_cache
#'
#' @examples
#' query_candles_cache("VIX.CBOE", 1) 
#' @export
query_candles_cache <- function(instruments, active_contract=1, min_active_contract=1, 
                                roll_pattern=NULL, start=NULL, stop=lubridate::now(), 
                                schedule=NULL, config=list(no_cache=T, no_clean=T, no_save=T, 
                                                           custom_roll=NULL, roll_same_day_all_legs=F)) {
  instruments <- query_instruments(instruments)
  if(!is.null(roll_pattern)) {
    instruments$active_contract <- roll_pattern
  }
  dir.create("~/bt_cache", showWarnings=F)
  cache_name <- paste0("~/bt_cache/", paste.list(unique(instruments$instrument_id), sep="_"))
  path <- cache_name
  #browser()
  ilog("query_candles_cache  ", cache_name, "start", as.character(start), "stop", as.character(stop))
  if(config$no_cache || !file.exists(path)) {
    q <- query_candles(instruments, active_contract = active_contract, 
                       min_active_contract=min_active_contract, start=start, stop=stop, custom_roll=config$custom_roll, 
                       roll_same_day_all_legs=config$roll_same_day_all_legs)
    data <- q %>% fetch_all()
    data_raw <- q$data
    if(!config$no_save) {
      ilog("raw saved ", path)
      saveRDS(data, path)
    }
    if(!config$no_clean) {
      # FIXME: load_trade_schedule will FAIL if nrow(instruments)>1
      if(is.null(schedule)) {
        schedule <- load_trade_schedule(instruments$instrument_id, start = start, end=stop, exclude = FALSE)
      }
      q$sessions <- schedule
      data<-data %>% map(~ clean.chunk(., schedules=list(schedule), cut_minutes=0, negative_bidask=T))
      if(!config$no_save) {
        ilog("cleaned saved ", path)
        saveRDS(data, path)
      }
    }
  }else {
    q <- new.env();
    ilog("reading ",path)
    data <- readRDS(path)
  }
  q$data <-data
  return(q)
}

#' chunk.resample
#' 
#' @export
ohlc<-function (data, freq=days(1), tz_offset=0, high="high", low="low", close="close") {
  data$datetime <- ((data$datetime+freq-hours(tz_offset)) %>% trunc_freq(freq))+hours(tz_offset)
  if(!has_name(data, high))
    high = close
  if(!has_name(data, low))
    low = close
  dots <- c(
    high = paste0("max(",high,")"), 
    low = paste0("min(",low,")"),
    close = paste0(close,"[n()]"))
  r <- data %>% 
    group_by(datetime, symbol) %>% arrange(datetime) %>% 
    summarise_(.dots = dots) %>% 
    as_data_frame() %>% arrange(datetime)
  #browser()
  r
}

#'
#'
#' @export
to_freq <- function(data, freq=days(1), tz_offset=0, by=c("datetime","symbol")) {
  data$datetime <- ((data$datetime+freq-hours(tz_offset)) %>% trunc_freq(freq))+hours(tz_offset)
  data %>% group_by_(.dots = by) %>% arrange_(.dots=by) %>% filter(row_number()==n())
}