library(rticks)
#library(devtools)
#load_all("~/rticks",export_all=TRUE)

# fetch symbol from 'symbols' table
ticker <- "VIX.CBOE.M2016"
cat("fetching ", ticker)
s(ticker) %>% fetch
