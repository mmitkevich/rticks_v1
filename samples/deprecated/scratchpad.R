library(rticks)

chunks <- query_candles("VIX",start = dt(2016)) %>% fetch_all

