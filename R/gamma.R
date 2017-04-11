#' add metrics
#' 
#' @export
metrics.gamma <- function(env) {
  #params = attr(perfs, "params")
  
  qtys <- env$perfs %>% spread(metric, value) %>% as_data_frame()
  if(has_name(qtys, "commission"))
    qtys <- qtys %>% select(-commission)
  qtys <- qtys %>% inner_join(env$params %>% select(symbol, spread, multiplier, commission), by="symbol")
  qtys <- qtys %>% mutate(
    rpnl = pmin(qty_buy, qty_sell)*spread*multiplier, 
    commission = commission*(qty_buy+qty_sell),
    pnl = pnl - commission) %>% 
    select(-spread,-multiplier)
  qtys<- qtys %>% gather(metric, value, -datetime, -symbol)
  qtys
}