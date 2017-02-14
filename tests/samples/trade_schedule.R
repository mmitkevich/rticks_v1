library(rticks)

instrumentID <- "FOAT.EUREX"
instrumentID <- "VIX.CBOT"



scheduleTimeIntervals <- query_schedule(instrumentID)

intervalDataFrame <- if(length(which(scheduleTimeIntervals$week_day == 0)) == 1) {
  # for this case use 0 interval for all buziness days
  week_day <- c(1,2,3,4,5,6)
  intervals <- rep(scheduleTimeIntervals$intervals, 6)
  data.frame(cbind(week_day,intervals))
} else {
  # for this case take buziness days intervals
  scheduleTimeIntervals %>% filter(week_day %in% c(1,2,3,4,5,6)) %>% select(week_day, intervals)
}


data <- head(chunks[[1]]$bid)
dateTimeTestVector <- c(data$bid$datetime[1:100], data$bid$datetime[19000:19100])

intervalsList <- intervalDataFrame$intervals %>% map(function(x) fromJSON(x))
names(intervalsList)







wday(dateTimeTestVector)
hour(dateTimeTestVector)
minute(dateTimeTestVector)

# TODO check minutes to delete from time interval
# TODO check advanced get quotes functions

FilteringRawPriceDF <- function(priceDF) {
  Prices <- priceDF
  Prices <- Prices[!duplicated(Prices$datetime),]
  Prices <- Prices[!is.na(Prices$close_bid) & !is.na(Prices$close_ask), ]
  Prices <- Prices[which(Prices$close_bid != 0 & Prices$close_ask != 0), ]
  Prices <- Prices[which(Prices$close_bid <= Prices$close_ask), ]
  return(Prices)
}
query_instruments()

# 0 NA duplicates negativeSpread time <0 <0 


chunks <- query_candles("PL", start = as_dt(16)) %>% fetch_all()
chunks[[1]]$mapping

workingChunk <- chunks[[1]]$bid[1:2000,]
hours <- hour(workingChunk$datetime)
minutes <- minute(workingChunk$datetime)

intervalStart <- "2008-05-27T17:00:00.000-05:00"
intervalEnd <- "2008-05-28T16:15:00.000-05:00"
# for 28

OlsonNames()
now("GMT")
now("America/Chicago")

files <- list.files(path = "/usr/local/share/exante-stat-schedule-data/") 
for (i in 1:length(files)) {
  path <- paste0("/usr/local/share/exante-stat-schedule-data/",files[i])
  data <- fromJSON(path)
  print(files[i])
  print(unique(data$intervals$tag))
}

# from 17:00 to 16:00 for Chicago

exanteID <- "VIX.CBOE.H2017"
exanteID <- "ES.CME.H2017"

mapping_file = function(exanteID) {
  mappingFile <- fromJSON("/usr/local/share/exante-stat-schedule-data/.mapping")
  fileName <- mappingFile$mapping$schedule[-47][sapply(mappingFile$mapping$match[-47], function(x) grepl(x, exanteID))]
  return(fileName)
  #TODO do something with error in 47 line in mapping regexp
}

get_historical_schedule <- function(exanteID,
                                    start = NULL,    # means earliest possible 
                                    stop = now()) {
  
  fileName <- mapping_file(exanteID)
  fullFileName <- paste0("/usr/local/share/exante-stat-schedule-data/", fileName, ".json")
  rawHistoricalSchedule <- fromJSON(fullFileName)
  if (is.null(start) == TRUE) {
    startDateTime <- as_datetime(min(rawHistoricalSchedule$intervals$start))
  } else {
    startDateTime <- start
  }
  historicalSchedule <- rawHistoricalSchedule$intervals %>% filter(tag != "premarket") %>% filter(tag != "after-hours") %>% 
    filter(session <= stop) %>% filter(session >= startDateTime) %>% select(start, end)
  return(historicalSchedule)
}

sc <- get_historical_schedule(exanteID="VIX.CBOE.M2017", start = as_datetime("2017-01-01"))

head(historicalSchedule)


