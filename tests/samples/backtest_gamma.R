library(rticks)

data <- data_frame(
  datetime = c(1,     2,      3,       4), 
  bid      = c(100,   101,    102,   103)
) %>% mutate(ask = bid+1, high=bid, low=ask)

pars <- data_frame(
  buy_gamma = 1, 
  sell_gamma = 1,
  buy_limit = c(1e6),
  sell_limit = c(-1e6),
  TP = 1)

init_pos = 0

results <- backtest.gamma(
  data, 
  init_pos = 0, 
  pars = pars,
  instruments = data_frame(mpi = 1)
)
