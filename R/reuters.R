ifply <- function(.x, .f, .p=function()T,...) {
  if(as_function(.p)()==T) {
    .f(.x,...)
  }else{
    .x
  }
}

.filter_schedule <- function(schedule, start=NULL, stop=NULL) {
  s <- schedule 
  if(!is.null(start))
    s <- s %>% filter(lead(datetime)>start)
  if(!is.null(stop))
    s <- s %>% filter(datetime<=stop)
  return(s)  
}

.timeline <- function(.x) unique(.x$datetime)

query_data.reuters <- function(.x=NULL, 
                               key="instrument_id", 
                               schedule=NULL, 
                               start=NULL, 
                               stop=lubridate::now(), 
                               where=NULL) {
  schedule <- ifnull(schedule, .x %>% parse_symbols(key=key) %>% 
                       roll_schedule(start=start, stop=stop))
  schedule <- schedule %>% .filter_schedule(start=start, stop=stop)
  q <- structure(list(
    con = sql.connect(),
    schedule = schedule,
    stop = stop,
    start = ifnull(start, .timeline(schedule)[1]),
    where = where
  ), class="reuters")
  return(q)
}

print.reuters <- function(q) {
  cat("start: ", as.character(q$start),
      ", stop:", as.character(q$stop), 
      ", symbols: ", q$schedule$instrument_id %>% unique %>% paste,
      ", fin: ", as.character(.timeline(q$schedule)[[2]]))
}

grouped_df_as_list <- function(.d) (.d %>% do(.out=(.)) %>% select(.out)) [[1]]
  
fetch.reuters <- function(q, fields=c("s.exante_id", "m.datetime", "m.close_bid", "m.close_ask", "m.high_bid", "m.low_ask")) {
  if(q$start>=q$stop)
    return(NULL)
  stop <- ifelse(nrow(q$schedule)>=2, q$schedule$datetime[2], q$stop)
  symbols <- (q$schedule %>% group_by(exante_id) %>% summarise(datetime=head(datetime,1)))$exante_id
  w <- Reduce(sql.and, c(
    "m.ric=s.ric",
    "m.datetime BETWEEN s.fut_first_trade_dt AND s.last_tradeable_dt",
    paste("s.exante_id","IN","(",paste.list(paste0("'",symbols,"'"),sep=","),")"),
    paste("m.datetime", "BETWEEN", 1000*as.numeric(q$start), "AND", 1000*as.numeric(stop)),
    q$where
  ))
  q$schedule <- q$schedule %>% tail(-1)
  q$start <- stop
  dfs <- sql.select(
      "quant_data.symbols s, quant_data.minutes m", 
      fields = fields, 
      where = w, 
      order = "datetime") %>%
    transmute(
      exante_id = exante_id, 
      datetime = as_datetime(datetime/1000), 
      bid=close_bid, 
      ask=close_ask, 
      high=high_bid, 
      low=low_ask) %>% 
    group_by(exante_id) %>% grouped_df_as_list()
  c("bid", "ask", "high", "low") %>% map(function(.key) {
    dfs %>% map(~ .x %>% select_(.dots=c("exante_id", "datetime", .key)) %>% rename_(.dots=setNames(.key,"")))
  })
  
  df <- dfs %>% reduce(function(.a, .b) full_join(.a, .b, by="datetime"))
  return(df)
}