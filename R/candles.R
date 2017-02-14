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
clean.chunk <- function(chunk, 
                        time_filter = NULL, 
                        value_filter = NULL ) {
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
