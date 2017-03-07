#' query candles from candles provider
#' 
#' @export

query_candles <- function(instruments = NULL, 
                          active_contract = seq(1,3), 
                          start = NULL, 
                          stop = lubridate::now(), 
                          schedule = NULL, 
                          provider = "reuters", ...) {
  query_fn <- get(paste0("query_candles.", provider))
  query_fn(instruments = instruments, 
           active_contract = active_contract,
           start = start,
           stop = stop,
           schedule = schedule,...)
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

#' cache_path
#' 
#' @export
cache_path <- function(instrument_id, start, active_contract, cache_dir="~/rticks/tests/"){
  paste0(cache_dir,instrument_id,".",as.character(active_contract),".rds")
}

#' query_candles_cache
#'
#' @examples
#' query_candles_cache("VIX.CBOE", 1) 
#' @export
query_candles_cache <- function(instruments, start=NULL, active_contract=1, no_cache=F, no_clean=F, no_save=F) {
  instruments <- query_instruments(instruments)
  cache_name <- paste(unique(instruments$instrument_id), sep="_")
  path <- cache_path(cache_name, start, active_contract)
  if(no_cache || !file.exists(path)) {
    cat("querying ", instrument_id, "\n")
    q <- query_candles(instruments, start=start, active_contract = active_contract)
    data <- q %>% fetch_all()
    data_raw <- data
    if(!no_save) {
      cat("raw saved ", path, "\n")
      saveRDS(data, path)
    }
    if(!no_clean) {
      schedule <- load_trade_schedule(instrument_id = instrument_id, start = start, exclude = FALSE)
      data<-data %>% map(~ clean.chunk(., schedule, cut_minutes=3, negative_bidask=T))
      if(!no_save) {
        cat("cleaned saved ", path, "\n")
        saveRDS(data, path)
      }
    }
  }else {
    data <- readRDS(path)
  }
  return(data)
}

#' chunk.resample
#' 
#' @export
chunk.resample<-function (data, freq="days") {
  data %>% 
    transmute(
      datetime=as_date(trunc(data$datetime, freq))
    ) %>% 
    arrange(datetime) %>%
    group_by(datetime, symbol) %>%
    filter(row_number()==n()) %>% 
    as_data_frame()
}
