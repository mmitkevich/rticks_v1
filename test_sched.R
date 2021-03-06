
contracSpecMonth <- data.frame(Number = c( 1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12),
                               Letter = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                               stringsAsFactors = FALSE)


roll_sched <- roll_schedule(instruments = c("GC.COMEX", "LH.CME"),
                            symbols = NULL,
                            active_contract = list(GC.COMEX=c(2,8,10,12),LH.CME = c(4,6,10,12)), # list(GOLD.FORTS=c(3,6,9,12), PL.NYMEX=c(3,7))
                            max_active_contract = list(GC.COMEX=4,LH.CME = 3),
                            min_active_contract = list(GC.COMEX=2,LH.CME = 2),
                            custom_roll = NULL, # ~ . - days(day(.)) - months(2)
                            start = as_datetime("2015-01-01"),
                            stop = NULL,
                            nm = "instrument_id",
                            fields=c("instrument_id", "exante_id", "month", "year", "first_notice_day"))

roll_sched_LH <- roll_schedule(instruments = "LH.CME",
                            symbols = NULL,
                            active_contract = list(LH.CME = c(2,4,5,6,7,8,10,12)), # list(GOLD.FORTS=c(3,6,9,12), PL.NYMEX=c(3,7))
                            max_active_contract = list(LH.CME = 5),
                            min_active_contract = list(LH.CME = 3),
                            custom_roll = NULL, # ~ . - days(day(.)) - months(2)
                            start = as_datetime("2015-01-01"),
                            stop = NULL,
                            nm = "instrument_id",
                            fields=c("instrument_id", "exante_id", "month", "year", "first_notice_day"))

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
    #  min_active_contract[[ins$instrument_id]]-1
    rs <- seq(0, max_active_contract[[ins$instrument_id]]) %>% 
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
    rs2<- bind_rows(rs0, rs1) %>% as_data_frame() %>% filter(active_contract>=min_active_contract[[ins$instrument_id]]-1)
    rs2
  })
  #browser()
  result <- bind_rows(result$.out) %>% arrange(datetime)
  result <- result %>% left_join(instruments%>%select(-exante_id, -active_contract), by="instrument_id")
  result2 <- schedule.roll.logic(result,instruments,min_active_contract,max_active_contract)
  result2
}




instruments <- query_instruments(c("GC.COMEX", "LH.CME"))
b <- schedule.roll.logic(roll_sched,instruments,min_active_contract,max_active_contract)





b <- schedule.roll.logic(sched, 
                         instruments  = instruments, 
                         min_active_contract = list(GC.COMEX=2,LH.CME = 2), 
                         max_active_contract = list(GC.COMEX=4,LH.CME = 3))


#######################################
sched <- roll_sched

#######################################
value <- min_active_contract
keys <- instruments$instrument_id
default <- 1


value_as_list <- function(value, keys, default){
  keys<-as.list(keys)
  defs <- keys %>% map(~ ifelse(is.list(value), default, value)) %>% setNames(keys)
  if(is.list(value))
    defs%>%modifyList(value)
  else
    defs
}



#######################################


schedule.roll.logic <- function(sched, instruments, min_active_contract, max_active_contract) {
  if(!is.data.frame(instruments) || !has_name(instruments, "active_contract"))
    instruments <- instruments %>% query_instruments()
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