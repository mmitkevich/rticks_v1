# clean_chunk_ sample

startDateTime <- as_datetime("2016-01-01")
endDateTime = as_datetime("2016-03-01")

chunks <- query_candles("VIX", start = startDateTime) %>% fetch_all()
chunk <- chunks[[1]]

schedule <- .get_historical_schedule(exanteID = "VIX.CBOE.M2016", start = startDateTime, end = endDateTime, exclude = FALSE)
cut_minutes <- 3

clean.chunk(chunk, 
            schedule, 
            cut_minutes, 
            negative_bidask = TRUE)