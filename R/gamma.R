#' add metrics
#' 
#' @export
metrics.gamma <- function(env, no_commission=F) {
  #params = attr(perfs, "params")
  #browser()
  qtys <- env$perfs %>% spread(metric, value) %>% as_data_frame()
  if(has_name(qtys, "commission"))
    qtys <- qtys %>% select(-commission)
  qtys <- qtys %>% inner_join(env$params %>% select(symbol, spread, multiplier, commission), by="symbol")
  qtys <- qtys %>% mutate(
    rpnl = pmin(qty_buy, qty_sell) * spread * multiplier)
  
  if(!no_commission)
    qtys <- qtys %>% mutate(
      rpnl = rpnl - commission, 
      commission = commission*(qty_buy+qty_sell),
      pnl = pnl - commission,
      pnl_high = pnl_high - commission,
      pnl_low = pnl_low - commission)
  else
    qtys <- qtys %>% mutate(commission = 0)
  qtys <- qtys %>% mutate(      
    drisk = (lag(pos)+0.5*(lag(price)-env$params$stop.buy)/env$params$mpi*env$params$gamma.buy)*(lag(price)-env$params$stop.buy),
    rtn = (pnl-lag(pnl))/drisk)
  qtys<- qtys %>% select(-spread,-multiplier) %>% gather(metric, value, -datetime, -symbol)
  qtys
}