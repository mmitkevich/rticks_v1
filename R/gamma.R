#' add metrics
#' 
#' @export
metrics.gamma <- function(env, no_commission=F, currency=NULL) {
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
  p <- env$params
  qtys <- qtys %>% mutate(      
    #drisk = (pos+0.5*(min(price,p$limit.buy)-p$stop.buy)/p$mpi*p$gamma.buy)*(price-p$risk.buy),
    drisk = (((min(price,p$limit.buy)-p$stop.buy)/p$mpi+1)*((min(price,p$limit.buy)-p$stop.buy)/2*p$gamma.buy+(p$stop.buy-p$risk.buy))+pos*(price-p$risk.buy))*p$multiplier,
    rtn = (pnl-lag(pnl))/lag(drisk))
  qtys<- qtys %>% select(-spread,-multiplier) %>% gather(metric, value, -datetime, -symbol)
  qtys
}