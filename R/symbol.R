# querying symbols from SQL

#' print symbol
#' 
#' @examples 
#'   symbols("VIX") %>% print()
#' @export
print.symbol <- function(self, ...) cat(as.yaml(self), "\n")

#' contract months
#'                           1   2   3   4   5   6   7   8   9  10  11  12
#' @export
contract_month_letter <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")


#' fetch symbol from SQL database
#' 
#' @examples
#'   fetch(s("VIX.CBOE.M2016")) # will fetch all fields from the symbol database
#' @export 
fetch.symbol <- function(self, start=NULL, stop=lubridate::now("GMT"), ...) {
  query_symbols(self,start,stop,...)
}

#' make sql AND expression
#' @export
sql.and <- function(x,y) {
  if(is.null(x))
    return(y);
  if(is.null(y))
    return(x);
  paste0("(",paste(x,"AND",y),")")
}

#' make sql OR expression
#' @export
sql.or <- function(x,y) {
  if(is.null(x))
    return(y);
  if(is.null(y))
    return(x);
  paste0("(",paste(x,"OR",y),")")  
}

#' return f(x) 
#' @export
ifnull <- function(x, default, modified=x) {
  if(is.null(x))
    return(default)
  return(modified)
}

#'
#' @export
.sql.match_id <- function(x, name, f.prefix=T) {
  #nnmap(x, function(x) sql.eq_or_like(ifelse(f.prefix, paste0(x,"%"),x), name=name))
  Reduce(sql.or, Map(function(x) sql.eq_or_like(ifelse(f.prefix, paste0(x,"%"),x), name=name), x))
}

#' 
#' @export
mutate_by <- function(.df, .f) {
  modifyList(.df, .f(.df))
}

#' parse symbol like VIX.CBOE.G2017 
#' @return (exante_id=VIX.CBOE.G2017, instrument_id=VIX.CBOE, exchange=CBOE, ticker=VIX, month=2, year=2017)
#' @export
parse_symbols <- function(.d, nm="exante_id") {
  .d <- as.data.frame(.d, nm=nm, stringsAsFactors=F)
  id <- .d[[nm]]
#  print(id)
  q <- strsplit(id, "\\.")
  .d$ticker <- q %>% map_chr(~ .x[1])
  .d$exchange <- q %>% map_chr(~ .x[2])
  .d$instrument_id <- paste0(.d$ticker,".",.d$exchange)
  future_part <- q %>% map_chr(~ .x[3])
  #print(future_part)
  .d$month <- substr(future_part, 1, 1)
  .d$month <- match(.d$month, contract_month_letter)
  #ifelse(is.na(future_part), NA, which.max(contract_month_letter==substr(future_part,1,1)))
  .d$year <- as.numeric(substr(future_part,2,5))
  option_part <- q %>% map(~ .x[4])
  option_type <- substr(option_part,1,1)
  .d$instrument_class = ifelse(!is.na(option_type), option_type,
                               ifelse(!is.na(.d$month), "F", "S"))
  .d$strike <- as.numeric(gsub("_","\\.",substr(option_part, 2, nchar(option_part))))
  .d 
}

#' query contracts meta-data from sql "quant_data.symbols"
#'
#' @param df       vector of instrument patterns (should match exante_id)
#'                 If data.frame was specified, its df[[nm]] will be used as instrument pattern.
#' @param nm       which column to use for instrument patterns, if df parameter is data.frame.
#' @param start    min(first notice day), or NULL (by default) to return everything
#' @param stop     max(first notice day), or NULL (by default) to return everything
#' @param where    additional SQL where conditions (will be AND-ed into the SQL query)
#' @param f.prefix TRUE if df contains instrument patterns, FALSE if it is exact exante_id
#' @param fields   list of fields to be returned
#' @param fields.dt list of datetime fields, would be converted into POSIXct automatically
#' @return         data.frame with symbols
#' 
#' @examples                  
#'  > query_symbols()
#'  > c("VIX.CBOE","PL.NYMEX") %>% query_symbols(start=now())
#'  > query_symbols(start = now() + months(1))
#'  > query_symbols("ZW", start=now())
#'     exante_id ric fut_notice_first ticker exchange instrument_id month year instrument_class strike
#' ZW.CBOT.H2017 WH7       2017-02-28     ZW     CBOT       ZW.CBOT     3 2017                F     NA
#' ZW.CBOT.K2017 WK7       2017-04-28     ZW     CBOT       ZW.CBOT     5 2017                F     NA
#' ...
#'  > c("VIX.CBOE","PL.NYMEX") %>% parse_symbols() %>% query_symbols(start=now())
#'
#' @export 
query_symbols<-function(df = NULL, 
                        nm = "exante_id", 
                        start = NULL, 
                        stop = NULL, 
                        where = NULL, 
                        f.prefix = T,
                        fields = list("exante_id", "ric", "fut_notice_first"), 
                        fields.dt = list("fut_notice_first", "fut_first_trade_dt", "last_tradeable_dt", "fut_dlv_dt_first", "fut_dlv_dt_last")) {
  # convert to data.frame
  df <- parse_symbols(df)
  # prepare WHERE
  w <- c(
    df[[nm]] %>% nnmap( ~ .sql.match_id(.x, nm, f.prefix)), # exante_id = 'XYZ' or exante_id = 'ABC' ...
    start %>% nnmap( ~ paste("fut_notice_first", ">=", trunc(as.numeric(.x)*1000))), # start date
    stop %>% nnmap( ~ paste("fut_notice_first", "<", trunc(as.numeric(.x)*1000))), # stop date
    where
    ) %>% reduce(sql.and)
  # proceed with SQL
  result <- sql.select("quant_data.symbols", 
             fields = fields,
             where = w,
             order = "fut_notice_first")
  # fix datetime columns
  if(nrow(result)>0 && !is.null(fields.dt)) {
    if(!is.null(fields))
      fields.dt<-intersect(fields.dt, fields)
    for(f in fields.dt)
      result[[f]] <- as.datetime(result[[f]])
  }
  # return result
  result
}

#' query quant_data
#' @examples 
#'   query_instruments("VIX")
#' @export
query_quant_data <- function(x, table, nm, fields = NULL, json_cols = NULL, f.prefix = T, ...) {
  x <- as.data.frame(x, nm=nm, stringsAsFactors=F)[[nm]]
  w <- .sql.match_id(x, name=nm, f.prefix=f.prefix)
  #result <- tryCatch(
  result <- sql.select(table, fields = fields, where = w, ...)
  #warning=function(w){})
  for(col in json_cols) {
    result[[col]] <- result[[col]] %>% map (fromJSON)
  }
  result
}

#' query_instruments
#' @examples 
#'  query_instruments("VIX")
#' @export

query_instruments <- function(x=NULL, nm = "instrument_id",
                             fields = c("instrument_id", "currency", "mpi", "comission_fixed", "multiplier", "active_contract"), 
                             json_cols = c("active_contract"), ...) {
  r <- query_quant_data(x, "quant_data.instruments", nm, fields=fields, json_cols=json_cols, ...)
  r$exante_id <- r[[nm]]
  return(r)
}

#' query_schedule
#' 
#' @examples
#'  query_schedule("NYMEX")
#' @export
query_schedule <- function(x=NULL, nm = "instrument_id", ...) {
  query_quant_data(x, "quant_data.schedule", nm=nm, ...)
}

#' roll_schedule
#' 
#' @export
roll_schedule <- function(instruments, 
                          active_contract = NULL, # list(GOLD.FORTS=c(3,6,9,12), PL.NYMEX=c(3,7))
                          max_active_contract = 12,
                          start = NULL,
                          stop = NULL,
                          nm = "instrument_id",
                          fields=c("instrument_id", "exante_id", "month", "year", "fut_notice_first")) {

  # load symbols including those expiring 1 year after the end of backtesting period so all the patterns could be built
  symbols <- instruments%>%parse_symbols() %>% 
    query_symbols(
      start = start,
      stop =  nnmap(stop, ~ . + years(1)))
  
  if(is.null(fields))
    fields <- names(r)
  if(is.null(start))
    start <- min(symbols$fut_notice_first, na.rm=T)
  
  # go through instruments
  result <- instruments %>% by_row(function(ins) {
      # get symbols for the instrument, conforming to active_contract pattern
      sym <- symbols %>% select_(.dots=fields) %>%
        filter(instrument_id==ins$instrument_id) %>%
        filter(month %in% ins$active_contract[[1]]) %>%
        arrange(fut_notice_first)
      
#      print(sym)
#      cat("----")
      
      # for each active month .a in [0..max_active] create clones of each row
      # datetime for .a cloned row is lagging .a rows before its source
      rs <- seq(0, max_active_contract) %>% 
        map_df( function(.active_contract) {
          sym %>% mutate(active_contract=.active_contract, datetime=lag(fut_notice_first, n=.active_contract))
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
  bind_rows(result$.out) %>% arrange(datetime)
}