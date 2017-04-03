
#' specify roll day of month and shift roll some months ahead of expiration
#' 
#' @examples
#' roll_day(day_of_month=2, months_ahead=2)
#'  will shift roll from expiration day 2016-03-17 to 2016-01-02 which is 2 months ahead and is 2nd of the month
#'   
#' @export
roll_day <- function(day_of_month=NA, months_ahead=NA) {
  function(dt) {
    if(!is.na(day_of_month)) 
      dt <- ifelse(day_of_month <= day(dt), dt - days(day(dt)) + days(day_of_month), dt-days(day(dt))+days(day_of_month)-months(1))
    if(!is.na(months_ahead))
      dt <- dt - months(months_ahead)
    as_datetime(dt)
  }
}

#'  value_as_list
#'  
#' @export
value_as_list <- function(value, keys, default){
  keys<-as.list(keys)
  defs <- keys %>% map(~ ifelse(is.list(value), default, value)) %>% setNames(keys)
  if(is.list(value))
    defs%>%modifyList(value)
  else
    defs
}

#' roll_schedule
#' 
#' @export
roll_schedule <- function(instruments,
                          symbols = NULL,
                          active_contract = NULL, # list(GOLD.FORTS=c(3,6,9,12), PL.NYMEX=c(3,7))
                          max_active_contract = 12,
                          min_active_contract = 1,
                          custom_roll = NULL, # ~ . - days(day(.)) - months(2)
                          start = NULL,
                          stop = NULL,
                          nm = "instrument_id",
                          fields=c("instrument_id", "exante_id", "month", "year", "first_notice_day")) {
  if(!is.data.frame(instruments) || !has_name(instruments, "active_contract"))
    instruments <- instruments %>% query_instruments()
  max_active_contract <- max_active_contract %>% value_as_list(instruments$instrument_id, 12)
  min_active_contract <- min_active_contract %>% value_as_list(instruments$instrument_id, 1)
  #  print(instruments)
  # lazy instruments loading
  ilog("roll_schedule",paste(instruments$instrument_id),
      "active_contract",paste(active_contract), 
      "max_active_contract",max_active_contract)
  # load symbols including those expiring 1 year after the end of backtesting period so all the patterns could be built
  if(is.null(symbols))
    symbols <- instruments %>% query_symbols(start = start, stop = NULL) #nnmap(stop, ~ . + years(1))
  
  if(is.null(fields))
    fields <- names(r)
  if(is.null(start))
    start <- min(symbols$first_notice_day, na.rm=T)
  
  # go through instruments
  result <- instruments %>% by_row(function(ins) {
    roll_pattern <- ifnull(ins$active_contract, c(1,2,3,4,5,6,7,8,9,10,11,12), ins$active_contract[[1]])
    roll_pattern <- ifnull(active_contract[[ins$instrument_id]], roll_pattern, active_contract[[ins$instrument_id]])
    # get symbols for the instrument, conforming to active_contract pattern
    sym <- symbols %>% select_(.dots=fields) %>%
      filter(instrument_id==ins$instrument_id) %>%
      filter(month %in% roll_pattern)
    if(!is.null(custom_roll)) {
      sym$first_notice_day <- as_function(custom_roll)(sym$first_notice_day)
    }
    sym <- sym %>% arrange(first_notice_day)
    
    #      print(sym)
    #      cat("----")
    
    # for each active month .a in [0..max_active] create clones of each row
    # datetime for .a cloned row is lagging .a rows before its source
    rs <- seq(min_active_contract[[ins$instrument_id]]-1, max_active_contract[[ins$instrument_id]]) %>% 
      map_df( function(.active_contract) {
        sym %>% mutate(active_contract=.active_contract, datetime=lag(first_notice_day, n=.active_contract))
      }) %>% # and sort by datetime
      arrange(datetime)
    # take properly formed
    rs1 <- rs %>% filter(!is.na(datetime))
    # and initial active contracts without datetime (because it is absent from dataframe)
    # patch it by taking first row for each contract and setting datetime = start
    rs0 <- rs %>% filter(is.na(datetime)) %>% 
      group_by(exante_id) %>% filter(row_number()==1) %>% mutate(datetime=start)
    #      print("===== rs0")
    #      print(rs0)
    #      print("===== rs1")
    #      print(rs1)
    
    # return them combined
    rs2<-as.data.frame(bind_rows(rs0, rs1))
    #browser()
    return(as.data.frame(rs2))
  })
  #browser()
  result <- bind_rows(result$.out) %>% arrange(datetime)
  result <- result %>% left_join(instruments%>%select(-exante_id, -active_contract), by="instrument_id")
  browser()
  result2 <- schedule.roll.logic(result,instruments,min_active_contract,max_active_contract)
  result2
}

#' custom logic keeping contract until it goes out of allowed active_contract indexes range 
#'
#'
#' @export
schedule.roll.logic <- function(sched, instruments, min_active_contract, max_active_contract) {
  if(!is.data.frame(instruments) || !has_name(instruments, "active_contract"))
    instruments <- instruments %>% query_instruments()
  browser()  
  min_active_contract <- value_as_list(min_active_contract, instruments$instrument_id, 1)
  max_active_contract <- value_as_list(max_active_contract, instruments$instrument_id, 12)
  for (i in 1:nrow(instruments)) {
    filt_inst_schedule <- sched %>% filter(instrument_id == instruments$instrument_id[i])
    act_contracts = seq(max_active_contract[[i]], min_active_contract[[i]])
    for (j in 1:length(unique(filt_inst_schedule$datetime))) {
      contract <- act_contracts[if (j %% length(act_contracts) == 0) {length(act_contracts)} 
                                           else {j - (j %/% length(act_contracts)) * length(act_contracts)}]
      line <- filt_inst_schedule %>% filter(datetime == unique(filt_inst_schedule$datetime)[j]) %>% filter(active_contract == contract)
      if (j == 1) {
        DF <- line
      } else {
        DF <- rbind(DF, line)
      }
    }
    if (i == 1) {
      DF_fin <- DF
    } else {
      DF_fin <- rbind(DF_fin, DF)
    }
  }
  DF_fin %>% arrange(datetime)
}


#' unique datetimes
#' 
#' @export
timeline <- function(schedule, start=NULL) {
  schedule <- ifnull(start, schedule, schedule%>%filter(datetime>=start))
  unique(schedule$datetime)
}
