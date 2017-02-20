library(rticks)

pars <- data_frame(
  buy_gamma = 1, 
  sell_gamma = 1,
  buy_limit = c(1e6),
  sell_limit = c(-1e6),
  TP = 1)

instruments = data_frame(mpi = 1)

data <- data_frame(
  datetime = c(1,     2,      3,       4,       5,        6,         7), 
  bid      = c(100,   101,    102,   103,     102,      101,        100)
) %>% mutate(ask = bid+1, high=bid, low=ask)


init_pos = 0

results <- backtest.gamma(
  data, 
  init_pos = 0, 
  pars = pars,
  instruments = data_frame(mpi = 1)
)
