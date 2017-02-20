library(dplyr)

#' calculate (pnl, dd ,mdd, rpnl)
#' 
#' @param data = (datetime, bid, ask, high, low)
#' @param pos = initial position
#' @param pars = (gamma=d(pos)/d(price), buy_limit=17, sell_limit=-INF, )
#' @return 
#' 
#' @export
backtest.gamma <- function(
  data, 
  init_pos = 0, 
  pars = data_frame(
      buy_gamma = 1, 
      sell_gamma = 1, 
      buy_limit = Inf,
      sell_limit = -Inf, 
      TP = 6), 
  instruments = data_frame(mpi = 1)
) {
  initial <- list(
    buy  = (data$bid[1] + data$ask[1] - pars$TP)/2,
    sell = (data$bid[1] + data$ask[1] + pars$TP)/2,
    pos = init_pos
    )
  r1 <- data %>% mutate( 
    buy_gamma = 0,
    sell_gamma = 0
  ) %>% mutate(
    buy_gamma  = ifelse(ask < pars$buy_limit,  pars$buy_gamma),
    sell_gamma = ifelse(bid > pars$sell_limit, pars$sell_gamma),
    TP = pars$TP
  ) %>% mutate(
    buy = initial$buy,
    sell = initial$sell,
    pos  = init_pos
  ) 

  r2 <- r1 %>% mutate(  # buy side
    buy = lag(buy, default = initial$buy)
  ) %>% mutate(
    buy_qty = (pmax(buy - low, 0)) * buy_gamma,
    buy_avg = ifelse(low < buy, (buy + low)/2, NA)
  ) %>% mutate(
    pos = lag(pos, default = initial$pos) + buy_qty,
    buy = pmin(low - instruments$mpi, buy)
  ) %>% mutate(
    sell = buy + TP
  )
  
  r3 <- r2 %>% mutate(   # sell side
    sell = lag(sell, default = initial$sell)
  ) %>% mutate(
    sell_qty = (pmax(high - sell, 0)) * sell_gamma,
    sell_avg = ifelse(high > sell, (sell + high)/2, NA)
  ) %>% mutate(
    pos = lag(pos, default = initial$pos) - sell_qty,
    sell = pmax(high + instruments$mpi, sell)
  ) %>% mutate(
    buy = sell - TP
  ) 
  
  r4 <- r3 %>% mutate( # calc pnl
    rpnl = cumsum(sell_avg*sell_qty - buy_avg*buy_qty, rm.),
    pos = cumsum(buy_qty - sell_qty)
  )
  
  return(r4)
} 

