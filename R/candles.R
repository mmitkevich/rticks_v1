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

  