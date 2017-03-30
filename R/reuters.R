ifply <- function(.x, .f, .p=function()T,...) {
  if(as_function(.p)()==T) {
    .f(.x,...)
  }else{
    .x
  }
}

#' @export
.reuters.fields = c("high", "low", "bid", "ask")

#' @export
.reuters.fields.sql = c("exante_id", "datetime", "close_bid", "close_ask", "high_bid", "low_ask")


.filter_schedule <- function(schedule, start=NULL, stop=NULL) {
  s <- schedule 
  # datetime
  #----------
  # first_notice_day_1
  #   start
  # first_notice_day_2
  
  # we keep 1 first_notice_day BEFORE start in the schedule. If start==first_notice_day we keep only it.
  if(!is.null(start))
    s <- s %>% filter(datetime>=start || lead(datetime)>start)
  if(!is.null(stop))
    s <- s %>% filter(datetime<stop)
  return(s)  
}

#' query candles from reuters minutes sql database
#' @examples
#' @export
query_candles.reuters <- function(instruments = NULL, 
                                  schedule = NULL,
                               active_contract = seq(1,3),
                               custom_roll = NULL,
                               start = NULL, 
                               stop = lubridate::now(), 
                               where = NULL) {
  if(!is.data.frame(instruments) || !has_name(instruments, "active_contract"))
    instruments <- query_instruments(instruments)
  ilog("query_candles.reuters", paste(instruments$instrument_id, as.character(active_contract)), "start", as.character(start), "stop", as.character(stop))
  schedule <- schedule %>% 
    ifnull(cached_attr(instruments, "schedule", 
                       instruments %>% 
                         roll_schedule(max_active_contract=max(active_contract), custom_roll=custom_roll, start=start, stop=stop)))
  if(getOption("debug",F)){
    wlog("SCHEDULE")
    print(schedule)
    #browser()
  }
  schedule <- schedule %>% .filter_schedule(start=start, stop=stop)
  q <- structure(new.env(), class="reuters")
  with(q, {
    schedule = schedule
    stop = stop
    start = ifnull(start, timeline(schedule, start = start)[1])
    where = where
    active_contract = active_contract
    fields = .reuters.fields
    instruments = instruments
  })
  #if(getOption("debug"))
  #  print(q)
  return(q)
}

#print.reuters <- function(q) {
#  print(as.list(q))
#}

#' translate exante_id into continious id, adds instrument_id
#'
#' @export
to_virtual_id <- function(df, symbols) {
  df %>% 
    left_join(symbols %>% select(exante_id,active_contract,instrument_id), by="exante_id") %>% 
    mutate(virtual_id=paste0(instrument_id,".",active_contract))
}

.candles.attributes <- c("instruments", "start", "stop", "events")

#' fetch query
#' 
#' @export
fetch.reuters <- function(q) {
  if(q$start>=q$stop)
    return(NULL)
  tl = timeline(q$schedule, start=q$start)
  stop <- ifelse(length(tl)>1, tl[[2]], q$stop)

  symbols <- q$schedule %>% filter(datetime<=q$start) %>%  # take past events
    group_by(exante_id) %>%      # for each contract's group
      arrange(datetime) %>%      # sort by datetime
      filter(row_number()==n()) %>%  # and take last active_contract numbering 
      filter(active_contract %in% q$active_contract)
  
  wlog("fetch.reuters in", as.character(as_datetime(q$start)),"..", as.character(as_datetime(stop)), "exante_ids", paste.list(symbols$exante_id,sep=" "))
  
  if(nrow(symbols)==0) {
    ilog("\nEMPTY roll schedule:\n")
    print(q$schedule)
    stop(paste("no symbol for ac=", paste0(q$active_contract), "found in roll schedule","start",as.character(as_datetime(q$start)),"stop",as.character(as_datetime(stop))))
  }
  w <- c(
    paste("exante_id","IN","(",paste.list(paste0("'", symbols$exante_id, "'"),sep=","),")"),
    paste("datetime", "BETWEEN", 1000*as.integer(q$start), "AND", 1000*as.integer(stop)),
    q$where
  ) %>% reduce(sql.and)
  df <- 
    sql.select(
      "quant_data.smart_minutes", 
      fields = .reuters.fields.sql, 
      where = w, 
      order = "datetime")
  #browser()
  if(nrow(df)>0) {
    df <- df %>% transmute(
        exante_id = exante_id, 
        datetime = as_datetime(datetime/1000+59),  # actual time is close_time FIXME  
        bid = close_bid, 
        ask = close_ask, 
        high = high_bid, 
        low = low_ask) 
    df <- df %>% to_virtual_id(symbols)
    df <- df  %>% # gather_("event", "value", .reuters.fields)  %>%
      arrange(datetime) # todo: match("event",c("h","l","b","a")) so h,l before b,a
  }  
  attr(df, "symbols") <- symbols
  attr(df, "instruments") <- q$instruments
  attr(df, "start") <- q$start
  attr(df, "stop") <- as_datetime(stop)
  attr(df, "events") <- .reuters.fields
  
  attr(df, "class") <- c(attr(df, "class"), "chunk")
  q$start <- stop
  
  #result<-structure(result, class="chunk")
  #row.names(df) <- NULL
  wlog("fetch.reuters out", nrow(df),  "rows", as.character(head(df$datetime,1)), "..", as.character(tail(df$datetime,1)), "exante_id", head(df$exante_id,1), "virtual_id", head(df$virtual_id,1))
  return(df)
}

#' group chunk by events
#' 
#' @export
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
