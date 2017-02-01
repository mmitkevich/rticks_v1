#' backtest strategy
#' @examples
#' symbols <- c("VIX", "PL", "GD")
#' underlyings <- query_symbols(symbols, f.spot = T)
#' contracts <- query_symbols(symbols, f.fut = T) %>% as.cfglist
#' rolls <- roll_schedule(contracts, patterns = contracts)
#' backtest()
#' @export
backtest <- function(symbols, setup) {
  
}