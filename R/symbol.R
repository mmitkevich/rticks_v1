#                           1   2   3   4   5   6   7   8   9  10  11  12
#' @export
contract_month_letter <- c("F","G","H","J","K","M","N","Q","U","V","X","Z")


#' print symbol
#' @examples 
#'   symbols("VIX") %>% print()
#' @export
print.symbol <- function(self, ...) cat(as.yaml(self), "\n")

#' fetch symbol from SQL database
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
ifelsenull<-function(x,f,true,fals) {
  if(is.null(x))
    return(NULL)
  if(f(x))
    return(true);
  return(false)
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
parse_symbols <- function(.d, key="exante_id") {
  .d <- as.data.frame(.d, nm=key, stringsAsFactors=F)
  id <- .d[[key]]
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

#' query futures from sql database
#' 
#' query actual futures contracts from 'quant_data.minutes' postgres table
#'
#' @param x        instrument_id
#' @param start    min(first notice day)
#' @param stop     max(first notice day)
#' 
#' @return         list of symbols, grouped by instruments
#' 
#' @examples                  
#'   query_symbols()
#'   c("VIX.CBOE","PL.NYMEX") %>% query_symbols(start=NULL)
#'   query_symbols(stop = now() + months(1))
#'   query_symbols("ZW", stop = as_dt(18))
#'   c("VIX.CBOE","PL.NYMEX") %>% parse_symbols() %>% query_symbols(start=now())
#' @export 
query_symbols<-function(x=NULL, key="exante_id", start=lubridate::now(), stop=NULL, where=NULL, 
                          f.prefix = T,
                          fields = list("exante_id", "ric", "fut_notice_first"), 
                          fields.dt = list("fut_notice_first", "fut_first_trade_dt", "last_tradeable_dt", "fut_dlv_dt_first", "fut_dlv_dt_last")) {
  x <- as.data.frame(x, nm=key, stringsAsFactors=F)
  id <- x[[key]]
  w <- Reduce(sql.and, c(
    nnmap(id, function(x) .sql.match_id(x, key, f.prefix)),
    nnmap(start, function(x) paste("fut_notice_first", ">=", trunc(as.numeric(x)*1000))),
    nnmap(stop, function(x) paste("fut_notice_first", "<", trunc(as.numeric(x)*1000))),
    where
    ))
  result <- sql.select("quant_data.symbols", 
             fields = fields,
             where = w,
             order = "fut_notice_first")
  if(nrow(result)>0 && !is.null(fields.dt)) {
    if(!is.null(fields))
      fields.dt<-intersect(fields.dt,fields)
    for(c in fields.dt)
      result[[c]]<-as.datetime(result[[c]])
  }
  #result <- result %>% as.cfglist("exante_id") %>% map(~ modifyList(.x, symbol(.x$exante_id)%>%flatten()))
  result <- result %>% parse_symbols()
  result
}

#' query quant_data
#' @examples 
#'   query_instruments("VIX")
#' @export
query_quant_data <- function(x, table, key, fields = NULL, json_cols = NULL, f.prefix = T) {
  x <- as.data.frame(x, nm=key, stringsAsFactors=F)[[key]]
  w <- .sql.match_id(x, name=key, f.prefix=f.prefix)
  #result <- tryCatch(
  result <- sql.select(table, fields = fields, where = w)
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

query_instruments <- function(x=NULL, key = "instrument_id",
                             fields = c("instrument_id", "currency", "mpi", "comission_fixed", "multiplier", "active_contract"), 
                             json_cols = c("active_contract"),
                             f.prefix = T) {
  query_quant_data(x, "quant_data.instruments", key, fields=fields, json_cols=json_cols, f.prefix=f.prefix)
}

#' query_schedule
#' @exampels
#'   query_schedule("NYMEX")
#' @export
query_schedule <- function(x=NULL, key = "instrument_id", f.prefix=T) {
  query_quant_data(x, "quant_data.schedule", key, f.prefix=f.prefix)
}

#' mutate list config
#' 
#' @examples
#'   symbols("GC","PL") %>% query_instruments() %>% mutate.cfg(roll_pattern=list(GC.COMEX=c(1,2,3)))
#' @export
mutate.cfg <- function(.x, ...) {
  list.kv <- function(key, value) { 
    ret <- list(); 
    ret[[key]] <- value
    ret
  }
  args <- list(...)
  Reduce(function(x, key) modifyList(x, args[[key]] %>% map(~ list.kv(key,.x))), names(args), .x)
}

#' convert list of lists to data.frame
#' @export
as_df <- function(.x) do.call(rbind.data.frame, .x)

#' roll_schedule
#' 
#' @export
roll_schedule <- function(instruments, 
                          active_contract = NULL, # list(GOLD.FORTS=c(3,6,9,12), PL.NYMEX=c(3,7))
                          max_active_contract = 3,
                          start = NULL,
                          stop = NULL,
                          fields=c("instrument_id", "exante_id", "month", "year", "fut_notice_first")) {

  # load symbols including those expiring 1 year after the end of backtesting period so all the patterns could be built
  symbols <- instruments$instrument_id %>% 
    query_symbols(
      start = start,
      stop =  nnmap(stop, ~ . + years(1)))
  
  if(is.null(fields))
    fields <- names(r)
  
  # go through instruments
  result <- instruments %>% by_row(function(ins) {
      # get symbols for the instrument, conforming to active_contract pattern
      s <- symbols %>% select_(.dots=fields) %>%
        filter(instrument_id==ins$instrument_id) %>%
        filter(month %in% ins$active_contract[[1]]) %>%
        arrange(fut_notice_first)
      
      # print(s)
      # cat("----")
      
      # for each active month .a in [0..max_active] create clones of each row
      # datetime for .a cloned row is lagging .a rows before its source
      rs <- seq(0, max_active_contract) %>% 
        map_df( function(.a) {
          s %>% mutate(active=.a, datetime=lag(fut_notice_first, n=.a))
        }) %>% # and sort by datetime
        arrange(datetime)
      # take properly formed
      rs1 <- rs %>% filter(!is.na(datetime))
      # and initial active contracts without datetime (because it is absent from dataframe)
      # patch it by taking first row for each contract and setting datetime = start
      rs0 <- rs %>% filter(is.na(datetime)) %>% 
                    group_by(exante_id) %>% filter(row_number()==1) %>% mutate(datetime=start)
      # return them combined
      bind_rows(rs0, rs1)
    })
  result$.out[[1]]
}