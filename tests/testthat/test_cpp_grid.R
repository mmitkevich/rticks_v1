library(rticks)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(testthat)
cfg <- config(backtest) %>% modifyList(list(
  no_cache = T, # всегда из базы
  no_save = F, # не писать кэш на диск
  log_level = LOG$DEBUG,
  log_stdout = LOG$DEBUG, 
  zero_position_on_rolls = F,
  zero_position_freq = F, #as.numeric(months(2)), 
  custom_roll = roll_day(day_of_month=1), # at 1st of the month, at least 1 month ahead of expiration  
  perfs_freq = as.numeric(minutes(1)),
  perfs_tz = as.integer(16)
))

params <- data_frame(
  # limits
  limit.buy     = 110,  # buy when price <= buy only.  NA. +Inf = buy always.  -Inf = buy never
  stop.buy      = 105,    # FIXME: no buy lower than 18
  
  limit.sell    = +Inf,  # sell when price>=sell only
  stop.sell     = +Inf,    # FIXME: no sell above 19
  pos           = 5,     # initial position
  
  spread        = 2,  # take profit
  
  gamma.buy     = 1,       # size to buy on each mpi
  gamma.sell    = 1,       # size to sell on each mpi (number of contracts)
  symbol        = "TEST.1",   # exante prefix of contract series
  min_active_contract = 1,
  active_contract = 1,             # which month to trade
  
  commission = 0,
  cash = 0,
  qty_buy = 0,
  qty_sell = 0,
  multiplier = 1,
  mpi = 1
)

prices = c(
  #113,114,
  #113,114,
  106,107,
  105,106,
  104,105,
  101,102,
  102,103,
  103,104,
  104,105,
  103,104,
  102,104,
  101,103,
  104,105,
  108,109,
  109,110,
  110,111,
  109,110,
  108,109,
  107,108,
  106,107,
  105,106,
  103,104,
  101,102,
  100,101,
  103,105,
  104,107,
  105,106,
  107,108,
  109,111,
  108,112,
  109,113,
  113,114,
  111,115,
  110,111,
  109,110,
  107,108,
  108,109,
  109,110,
  110,111,
  111,112,
  112,113
)

data <- data_frame(datetime = dt <- seq(1,length(prices)/2) %>% map_dbl( ~ as.numeric(ymd("2015-01-01")+ . * minutes(1))) %>% as_datetime(),
           bid = prices[seq(1,length(prices),2)],
           ask = prices[seq(2,length(prices),2)],
           virtual_id = "TEST.1")

rs <- backtest.chunk(data,params,"gamma",cfg)
r <- new.env()
r$perfs <- rs$perfs
r$perfs$datetime <- as_datetime(r$perfs$datetime)
r$params <- params
bt_reports(r)
#r$metrics <- r$metrics[-1,]
r$metrics %>% select(datetime, bid, ask, price_high, price_low, pos, pnl, rpnl, qty_buy, qty_sell, buy_high, buy_low, sell_high,sell_low) %>% View()
bt_plot(r)

grid.arrange(ggplot(r$metrics, aes(x=datetime)) +
  geom_abline(slope=0,intercept=params$limit.buy, colour="grey") +
  geom_abline(slope=0,intercept=params$stop.buy, colour="grey") +
  geom_step(aes(y=price_low), colour="red") + 
  geom_step(aes(y=price_high), colour="green") + 
  geom_step(aes(y=bid), colour="blue") + 
  geom_step(aes(y=ask), colour="magenta") + 
  theme_bw() +scale_y_continuous(breaks=seq(100,115)) + scale_x_datetime(date_minor_breaks = "1 min"),
  ggplot(r$metrics, aes(x=datetime)) + 
    geom_step(aes(y=pos)) + 
    theme_bw() + scale_x_datetime(date_minor_breaks = "1 min"))

bt_summaries(r)

spr <- r$metrics%>%mutate(spr=ask-bid) %$% spr
spr.max <- max(spr, na.rm = T)
spr.min <- min(spr, na.rm = T)

test_that("ask-bid==TP+1",  expect_equal(spr.max,3) & expect_equal(spr.min,3))
test_that("min(buy_low)==105", expect_equal(min(r$metrics$buy_low, na.rm=T), 105))
test_that("max(buy_high)==110", expect_equal(max(r$metrics$buy_high, na.rm=T), 110))
test_that("min(sell_low)==107", expect_equal(min(r$metrics$sell_low, na.rm=T), 107))
#test_that("max(sell_high)==112", expect_equal(max(r$metrics$sell_high, na.rm=T), 112))

test_that("max(bid)==110", expect_equal(max(r$metrics$bid, na.rm=T),110))
test_that("min(bid)==105", expect_equal(min(r$metrics$bid, na.rm=T),105))
test_that("max(ask)==112", expect_equal(max(r$metrics$ask, na.rm=T),112))
test_that("min(ask)==107", expect_equal(min(r$metrics$ask, na.rm=T),107))