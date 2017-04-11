library(rticks)
# configuration loaded from ~/.rticks.yaml
sym <- c("PL.NYMEX","GC.COMEX") %>% parse_symbols
sym
sym <- sym %>% query_symbols(start=as_dt(2015))
sym
ins <- query_instruments("PL.NYMEX")
ins
rollp <- roll_schedule(ins, sym)
rollp
ins$roll_pattern[[1]] <- c(4, 2)
roll_schedule(ins, sym)
