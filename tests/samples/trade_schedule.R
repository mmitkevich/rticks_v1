library(rticks)
library(lubridate)
library(magrittr)

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

FilteringRawPriceDF <- function(priceDF) {
  Prices <- priceDF
  Prices <- Prices[!duplicated(Prices$datetime),]
  Prices <- Prices[!is.na(Prices$close_bid) & !is.na(Prices$close_ask), ]
  Prices <- Prices[which(Prices$close_bid != 0 & Prices$close_ask != 0), ]
  Prices <- Prices[which(Prices$close_bid <= Prices$close_ask), ]
  return(Prices)
}

# 0 NA duplicates negativeSpread time <0 <0 


##############################################################################

.mapping_file = function(exanteID) {
  mappingFile <- fromJSON("/usr/local/share/exante-stat-schedule-data/.mapping")
  fileName <- mappingFile$mapping$schedule[sapply(mappingFile$mapping$match, function(x) grepl(x, exanteID, perl = T))]
  return(fileName)
}

.parseDateTime <- function(datetime) {
  parse_date_time(datetime, orders = "ymd HMOSz")
}


exanteID <- "VIX.CBOE.H2016"
start <- as_datetime("2016-01-01")
end <- as_datetime("2016-03-01")


.get_historical_schedule <- function(exanteID,
                                     start = NULL,    # means earliest possible 
                                     end = NULL,
                                     exclude = FALSE) {
  
  fileName <- .mapping_file(exanteID)
  fullFileName <- paste0("/usr/local/share/exante-stat-schedule-data/", fileName, ".json")
  rawHistoricalSchedule <- fromJSON(fullFileName)
  startDateTime <- ifnull(start, as_datetime(min(rawHistoricalSchedule$intervals$start)))
  endDateTime <-  ifnull(end, as_datetime(now()))
  historicalSchedule <- rawHistoricalSchedule$intervals %>% 
                        filter(tag != "premarket" & tag != "after-hours") %>%
                        filter(session <= endDateTime) %>% 
                        filter(session >= startDateTime) %>% 
                        select(start, end)
  
  if (exclude == TRUE) {
    n <- nrow(historicalSchedule)
    historicalSchedule <- historicalSchedule %>% 
                           mutate(start_1 = end, end_1 = lead(start)) %>% 
                           select(start_1, end_1) %>% 
                           rename(start = start_1, end = end_1) %>% 
                           filter(row_number() < n)
  }
  start <- as_datetime(sapply(historicalSchedule$start, .parseDateTime))
  names(start) <- NULL
  end <- as_datetime(sapply(historicalSchedule$end, .parseDateTime))
  names(end) <- NULL
  schedule <- data_frame(start = start, end = end)
  return(schedule)
}

.filter_chunk_by_schedule <- function(chunk, 
                                      excludeStart, 
                                      excludeEnd, 
                                      numberOfMinutesToCutAway) {
  filtChunk <- chunk %>% filter(datetime <= excludeStart - minutes(numberOfMinutesToCutAway + 1) | datetime >= excludeEnd + minutes(numberOfMinutesToCutAway + 1))
  return(filtChunk)
}






















# TO TEST
chunks <- query_candles("VIX", start = as_datetime("2016-01-01")) %>% fetch_all()
chunk <- chunks[[1]]
attributes(chunk)$start


sc <- .get_historical_schedule(exanteID = "VIX.CBOE.M2016", start = as_datetime("2016-01-03"), end = as_datetime("2016-03-01"), exclude = FALSE)

start <- min(chu$datetime)
end <- max(chu$datetime)
numberOfMinutesToCutAway <- 3


by_event.chunk(chunk)



a <- sc %>% gather(p, datetime) %>% mutate(p = ifelse(p == "start", 1, -1)) 
b <- data_frame(datetime = chunk$datetime, p = 0)
c <- bind_rows(a,b) %>% arrange(datetime)

d <- c %>% mutate(s = cumsum(p))
e <- d %>% filter(s > 0) %>% arrange(datetime)
f <- e %>% filter(p == 0)
g <- data_frame(datetime=unique(f$datetime), p = T)
g <- f %>% select(datetime) %>% left_join(chunk, by="datetime")

###################################################


clean.chunk <- function(chunk, 
                        schedule, 
                        cut_minutes = 0,
                        dedup_datetime = TRUE, 
                        negative_bidask = TRUE) {
  
  ########################################################################################################################
  # SCHEDULE
  start <- attributes(chunk)$start
  end <- attributes(chunk)$stop                                      # TODO change after MIM end <- start + months(2)
  exante_ids <- attributes(chunk)$mapping$exante_id
  
  schedule_list <- list()
  for (i in 1:length(exante_ids)) {
    schedule_list[[i]] <- .get_historical_schedule(exante_ids[i], start, end, exclude = FALSE)
  }
  unique_schedule_list <- unique(schedule_list)
  
  for (w in 1:length(unique_schedule_list)) {
    start <- unique_schedule_list[[w]]$start + minutes(cut_minutes)
    end <- unique_schedule_list[[w]]$end - minutes(cut_minutes)
    unique_schedule_with_minutes <- data_frame(start, end)
    schedWithParamDate <- unique_schedule_with_minutes %>% gather(p, datetime) %>% mutate(p = ifelse(p == "start", 1, -1)) 
    chunkWithParamDate <- data_frame(datetime = chunk$datetime, p = 0)
    bindedParamDate <- bind_rows(schedWithParamDate, chunkWithParamDate) %>% arrange(datetime)
    
    chunk_filtered <- bindedParamDate %>% mutate(s = cumsum(p)) %>% filter(s > 0 & p == 0) %>% 
                      distinct(datetime) %>% select(datetime) %>% left_join(chunk, by = "datetime")
    
    chunk <- chunk_filtered
  }
  
  ########################################################################################################################
  # DEDUP
  if (dedup_datetime == TRUE) {
    chunk_by_event <- by_event.chunk(chunk)
    chunk_by_event$high <- chunk_by_event$high[!duplicated(chunk_by_event$high$datetime ), ]
    chunk_by_event$low <- chunk_by_event$high[!duplicated(chunk_by_event$low$datetime ), ]
    chunk_by_event$bid <- chunk_by_event$high[!duplicated(chunk_by_event$bid$datetime ), ]
    chunk_by_event$ask <- chunk_by_event$high[!duplicated(chunk_by_event$ask$datetime ), ]
  }
  'chunk_by_event <- by_event.chunk(chunk)
  chunk_by_event$high <- chunk_by_event$high[!duplicated(chunk_by_event$high$datetime ), ]
  chunk_by_event$low <- chunk_by_event$high[!duplicated(chunk_by_event$low$datetime ), ]
  chunk_by_event$bid <- chunk_by_event$high[!duplicated(chunk_by_event$bid$datetime ), ]
  chunk_by_event$ask <- chunk_by_event$high[!duplicated(chunk_by_event$ask$datetime ), ]
  # TODO collect to one usual chunk
  # Plus negatice bid-ask spread'
}

by_usual.chunk <- function(df) {
  
}

by_event.chunk <- function(df) {
  dfs <- df %>% spread(event, value) %>% 
    group_by(virtual_id) %>% 
    grouped_df_as_list
  result <- setNames(nm=.reuters.fields) %>% 
    map(function(.col) {
      dfs %>% map(~ .x %>% 
                    select_(.dots = c("datetime", .col)) %>% 
                    rename_(.dots = setNames(.col, .x$virtual_id[[1]]))) %>% 
        reduce(~ full_join(.x, .y, by="datetime"))
    })
  for(a in .candles.attributes)
    attr(result,a) <- attr(df, a)
  result
}