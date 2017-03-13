# clean_chunk_ sample
library(rticks)

startDateTime <- as_datetime("2016-01-01")
endDateTime = as_datetime("2016-03-01")

chunk <- query_candles("VIX", start = startDateTime) %>% fetch


schedule <- load_trade_schedule(instrument_id = "VIX.CBOE", start = startDateTime, end = endDateTime, exclude = FALSE)
cut_minutes <- 3

cleaned<-clean.chunk(chunk, 
            schedules=list(schedule), 
            cut_minutes, 
            negative_bidask = TRUE)

cleaned