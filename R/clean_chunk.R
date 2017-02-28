library(rticks)
library(lubridate)
library(magrittr)

.mapping_file = function(exanteID) {
  mappingFile <- fromJSON("/usr/local/share/exante-stat-schedule-data/.mapping")
  fileName <- mappingFile$mapping$schedule[sapply(mappingFile$mapping$match, function(x) grepl(x, exanteID, perl = T))]
  return(fileName)
}

'.parseDateTime <- function(datetime) {
  parse_date_time(datetime, orders = "ymd HMOSz")
}'

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

###################################################

clean.chunk <- function(chunk, 
                        schedule, 
                        cut_minutes = 0, 
                        negative_bidask = TRUE) {
  
  ########################################################################################################################
  # SCHEDULE
  start <- attributes(chunk)$start
  end <- attributes(chunk)$stop
  exante_ids <- unique(chunk$exante_id)
  
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
  # NEGATIVE BID-ASK SPREAD
  if (negative_bidask == TRUE) {
    chunk <- chunk %>% filter(ask > bid)
  }
  ########################################################################################################################
  return(chunk)
}
