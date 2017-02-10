ifply <- function(.x, .f, .p=function()T,...) {
  if(as_function(.p)()==T) {
    .f(.x,...)
  }else{
    .x
  }
}

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

#' unique datetimes
#' 
#' @export
timeline <- function(schedule, start=NULL) {
  schedule <- ifnull(start, schedule, schedule%>%filter(datetime>=start))
  unique(schedule$datetime)
}

#' query candles from reuters minutes sql database
#' @examples
#' @export
query_candles.reuters <- function(instruments = NULL, 
                               schedule = NULL,
                               active_contract = seq(1,3),
                               start = NULL, 
                               stop = lubridate::now(), 
                               fields = c("bid", "ask", "high", "low"),
                               where = NULL) {
  if(is.null(schedule))
    schedule <- instruments %>% parse_symbols(nm="instrument_id") %>% 
      roll_schedule(max_active_contract=max(active_contract), start=start, stop=stop)
  schedule <- schedule %>% .filter_schedule(start=start, stop=stop)
  q <- structure(new.env(), class="reuters")
  with(q, {
    schedule = schedule
    stop = stop
    start = ifnull(start, timeline(schedule, start = start)[1])
    where = where
    active_contract = active_contract
  })
#  if(getOption("debug"))
    print(q)
  return(q)
}

#' print query
#' 
#' @export
print.reuters <- function(q) {
  cat("start: ", as.character(q$start),
      ", stop:", as.character(q$stop), 
      ", symbols: ", q$schedule$instrument_id %>% unique %>% paste,
      ", next: ", as.character(timeline(q$schedule, start = q$start)[[2]]),"\n")
}

#' translate exante_id into continious id, adds instrument_id
#'
#' @export
to_virtual_id <- function(instruments, mapping) {
  parsed <- parse_exante_id(instruments$exante_id)
  instruments %>% mutate(
    virtual_id = parsed$instrument_id %>% paste0(".", mapping$active_contract)
  )
}

#' fetch query
#' 
#' @export
fetch.reuters <- function(q, 
                      fields=c("s.exante_id", "m.datetime", "m.close_bid", "m.close_ask", "m.high_bid", "m.low_ask")) {
  if(q$start>=q$stop)
    return(NULL)
  tl = timeline(q$schedule, start=q$start)
  stop <- ifelse(length(tl)>1, tl[[2]], q$stop)
  #mapping <- q$schedule %>% group_by(exante_id) %>% 
  #               summarise(
  #                 datetime = head(datetime,1),
  #                 active_contract = head(active_contract,1),
  #                 instrument_id = head(instrument_id,1))
  mapping <- q$schedule %>% filter(datetime<=q$start) %>%  # take past events
    group_by(exante_id) %>%      # for each contract's group
      arrange(datetime) %>%      # sort by datetime
      filter(row_number()==n()) %>%  # and take last active_contract numbering 
      filter(active_contract %in% q$active_contract)
  
  w <- c(
    "m.ric=s.ric",
    "m.datetime BETWEEN s.fut_first_trade_dt AND s.last_tradeable_dt",
    paste("s.exante_id","IN","(",paste.list(paste0("'", mapping$exante_id, "'"),sep=","),")"),
    paste("m.datetime", "BETWEEN", 1000*as.numeric(q$start), "AND", 1000*as.numeric(stop)),
    q$where
  ) %>% reduce(sql.and)
  df <- 
    sql.select(
      "quant_data.symbols s, quant_data.minutes m", 
      fields = fields, 
      where = w, 
      order = "datetime") %>%
    transmute(
      exante_id = exante_id, 
      datetime = as_datetime(datetime/1000), 
      bid = close_bid, 
      ask = close_ask, 
      high = high_bid, 
      low = low_ask)
  df <- df %>%
    to_virtual_id(mapping)
  #browser()
  dfs <- df %>%
    group_by(virtual_id) %>% 
    grouped_df_as_list
  result <- setNames(nm=q$fields) %>% 
    map(function(.col) {
      dfs %>% map(~ .x %>% 
        select_(.dots=c("datetime", .col)) %>% 
        rename_(.dots=setNames(.col, .x$virtual_id[[1]]))) %>% 
      reduce(~ full_join(.x, .y, by="datetime"))
    })
  result$mapping <- mapping
  result$start <- q$start
  result$stop <- stop
  
  q$start <- stop
  result<-structure(result, class="chunk")
#  if(getOption("debug"))
    print(result)
  return(result)
}

#' print chunk
#' 
#' @export
print.chunk <- function(q) {
  cat("start: ", as.character(as_datetime(q$start)),
      ", stop:", as.character(as_datetime(q$stop)), 
      ", symbols: ", (q$mapping%>%arrange(active_contract))$exante_id %>% paste,"\n")
}

#' fetch all chunks
#' 
#' @export
fetch_all <- function(q) {
  chunk <- fetch(q)
  result <- list()
  i <- 1
  while(!is.null(chunk)) {
    result[[i]] <- chunk
    i <- i+1
    chunk <- fetch(q)
  }
  return(result)
}

#' rbind chunks
#' 
#' @export
rbind.chunks <- function(chs, fields = c("bid", "ask", "high", "low")) {
  setNames(nm=fields) %>% map(function(f) {
    chs %>% map( ~ .x[[f]] )  %>% bind_rows()
  })
}