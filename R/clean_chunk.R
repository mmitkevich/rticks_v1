#' load_trade_schedules
#' @export
load_trade_schedules <- function(instruments, start=NULL, end=NULL, exclude=F){
  schedule_list <- list()
  instr_ids <- unique(instruments$instrument_id)
  for (i in 1:length(instr_ids)) {
    schedule_list[[i]] <- load_trade_schedule(instr_ids[i], start=start, end=end, exclude = exclude)
  }
  schedules <- unique(schedule_list)
  schedules
}
 

#' load_trade_schedule
#' 
#' @examples 
#' load_trade_schedule("VIX.CBOE")
#' @export
load_trade_schedule <- function(instrument_id,
                                     start = NULL,    # means earliest possible 
                                     end = NULL,
                                     exclude = FALSE,
                                     path="/usr/local/share/exante-stat-schedule-data/"
                                     #path="~/exante-stat-schedule-data/"
                                ) {

  stopifnot(nrow(instrument_id)==1 || is.character(instrument_id) & length(instrument_id)==1)
  wlog("load_trade_schedule", paste(as.character(instrument_id)), "start", as.character(start), "end", as.character(end), "path", path)
  q <- strsplit(instrument_id, "\\.")
  ticker <- q %>% map_chr(~ .x[1])
  exchange <- q %>% map_chr(~ .x[2])
  future_part <- q %>% map_chr(~ .x[3])
  exanteID <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), 
                     paste0(ticker, ".", 
                            exchange, ".", 
                            substr(future_part, 0, 2),"/X0000-X0000^"), 
                     paste0(instrument_id,".X0000^"))
  mappingFile <- fromJSON(paste0(path,".mapping"))
  fileName <- head(mappingFile$mapping$schedule[sapply(mappingFile$mapping$match, function(x) grepl(paste0("^",x), exanteID, perl = T))], 1)
  fullFileName <- paste0(path, fileName, ".json")
  wlog("load_trade_schedule from", fullFileName)
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

.parseDateTime <- function(d) {
  #2005-01-04T02:00:00.000-06:00
  #paste0(substring(d,1, nchar(d)-3),substring(d,nchar(d)-1)) %>% strptime(format="%Y-%m-%dT%H:%M:%S%z")
  parse_date_time(d,"%Y-%m-%d %H:%M:%OS %z")
}
###################################################
#' cleans chunk of data
#' 
#' @examples
#'  
#' @export
clean.chunk <- function(chunk, 
                        schedules = NULL, 
                        cut_minutes = 0, 
                        negative_bidask = TRUE,
                        zero_prices = TRUE) {
  
  ########################################################################################################################
  # SCHEDULE
  start <- attributes(chunk)$start
  end <- attributes(chunk)$stop
  exante_ids <- unique(chunk$exante_id)

  ilog("clean.chunk ", paste(exante_ids), "start", as.character(start), "end", as.character(end),"\n")
  if(nrow(chunk)==0)
    return(chunk)
  if(is.null(schedules)) {
    schedules <- load_trade_schedules(parse_exante_id(exante_ids),start=start,end=end,exclude=F)
  }
  l.orig = nrow(chunk)
  for (w in 1:length(schedules)) {
    start <- schedules[[w]]$start + minutes(cut_minutes)
    end <- schedules[[w]]$end - minutes(cut_minutes)
    unique_schedule_with_minutes <- data_frame(start, end)
    schedule_with_datetime <- unique_schedule_with_minutes %>% gather(p, datetime) %>% mutate(p = ifelse(p == "start", 1, -1)) 
    chunk_with_datetime <- data_frame(datetime = chunk$datetime, p = 0)
    bindedParamDate <- bind_rows(schedule_with_datetime, chunk_with_datetime) %>% arrange(datetime)
    
    chunk_filtered <- bindedParamDate %>% mutate(s = cumsum(p)) %>% filter(s > 0 & p == 0) %>% 
                      distinct(datetime) %>% select(datetime) %>% left_join(chunk, by = "datetime")
    
    chunk <- chunk_filtered
  }
  wlog("clean.chunk ",exante_ids, "from", l.orig, "to",nrow(chunk),"rows")
  ########################################################################################################################
  # NEGATIVE BID-ASK SPREAD
  if (negative_bidask == TRUE) {
    chunk <- chunk %>% filter(ask > bid)
  }
  if(zero_prices==TRUE){
    chunk <- chunk %>% filter(ask!=0 & bid!=0)
  }
  ########################################################################################################################
  return(chunk)
}
