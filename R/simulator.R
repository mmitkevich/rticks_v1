library(dplyr)

#' calculate (pnl, dd ,mdd, rpnl)
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
  instruments = data_frame(mpi = 0.05)
) {
  initial <- c(
    buy  = (data$bid[1] + data$ask[1] - pars$TP)/2,
    sell = (data$bid[1] + data$ask[1] + pars$TP)/2,
    pos = init_pos
    )
  r<- data %>% mutate( 
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

  r <- r %>% 
  
  # buy side
  mutate(
    buy = lag(buy, default = initial$buy)
  ) %>% mutate(
    buy_qty = max(buy - low, 0) * buy_gamma,
    buy_avg = ifelse(low < buy, (buy + low)/2, NA)
  ) %>% mutate(
    pos = lag(pos) + buy_qty,
    buy = min(low - instruments$mpi, buy)
  ) %>% mutate(
    sell = buy + TP
  ) %>% 
  
  # sell side
  mutate(
    sell = lag(sell, default = initial$sell)
  ) %>% mutate(
    sell_qty = max(high - sell, 0) * sell_gamma,
    sell_avg = ifelse(high > sell, (sell + high)/2, NA)
  ) %>% mutate(
    pos = lag(pos) - sell_qty,
    sell = max(high + instruments$mpi, sell)
  ) %>% mutate(
    buy = sell - TP
  ) %>% mutate(
    rpnl = cumsum(sell_avg*sell_qty - buy_avg*buy_qty),
    pos = cumsum(buy_qty - sell_qty)
  )
} 

data <- data_frame(
  datetime = c(1,     2,      3,       4), 
  bid      = c(100,   103,    106,   109)
) %>% mutate(ask = bid+1, high=ask, low=bid)

pars <- data_frame(
  buy_gamma = 1, 
  sell_gamma = 1,
  buy_limit = c(1e6),
  sell_limit = c(-1e6),
  TP = 4)

init_pos = 0

results <- backtest.gamma(
  data, 
  init_pos = 0, 
  pars = pars,
  instruments = data_frame(mpi = 1)
  )
