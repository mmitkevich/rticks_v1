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


#' 
#' @export
mutate_by <- function(.df, .f) {
  modifyList(.df, .f(.df))
}

#' parses exante_id vector
#' 
#' @export
parse_exante_id <- function(id, instruments=NULL) {
  if(is.null(instruments))
    instruments = data.frame(exante_id=id)
  q0 <- strsplit(id, "\\.")
  
  q <- q0 %>% map(function(t) {
    if(tail(t,1)=="FX") {
      q <- c(t[1],t[2],"FX") #t[3]=="TOM" what to do?
    }
    t
  })
  instruments$ticker <- q %>% map_chr(~ .x[1])
  instruments$exchange <- q %>% map_chr(~ .x[2])
  future_part <- q %>% map_chr(~ .x[3])
  #print(future_part)
  instruments$month <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), 
                              ifelse(substr(future_part, 3, 3) == "/",substr(future_part, 4, 4),NA), 
                              substr(future_part, 1, 1))
  instruments$month <- match(instruments$month, contract_month_letter)
  #ifelse(is.na(future_part), NA, which.max(contract_month_letter==substr(future_part,1,1)))
  instruments$year <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), as.numeric(substr(future_part,5,8)), as.numeric(substr(future_part,2,5)))
  # month2 and year2 only for standalone spreads
  instruments$month2 <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), substr(future_part, 10, 10), NA)
  instruments$month2 <- match(instruments$month2, contract_month_letter)
  instruments$year2 <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), as.numeric(substr(future_part,11,14)), NA)
  instruments$instrument_id <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), 
                                      ifelse(is.na(instruments$month) == T & is.na(instruments$month2) == T,
                                             as.character(instruments$exante_id),
                                             paste0(instruments$ticker, ".", instruments$exchange, ".", substr(future_part, 0, 2), (as.numeric(instruments$year2) - as.numeric(instruments$year))*12 + (as.numeric(instruments$month2) - as.numeric(instruments$month)), "M")),
                                      ifelse(is.na(instruments$exchange), instruments$ticker, paste0(instruments$ticker,".",instruments$exchange)))
  
  option_part <- q %>% map(~ .x[4])
  option_type <- substr(option_part,1,1)
  instruments$instrument_class = ifelse(q0 %>% map_chr(~ tail(.,1)) == "FX", "FX",
                                        ifelse(substr(future_part, 0, 2) == "CS", "CS",
                                          ifelse(substr(future_part, 0, 2) == "RS", "RS",
                                               ifelse(!is.na(option_type), option_type,
                                                      ifelse(!is.na(instruments$month), "F", "S")))))
  instruments$strike <- as.numeric(gsub("_","\\.",substr(option_part, 2, nchar(option_part))))
  instruments
}
#' parse symbol like VIX.CBOE.G2017 
#' @return (exante_id=VIX.CBOE.G2017, instrument_id=VIX.CBOE, exchange=CBOE, ticker=VIX, month=2, year=2017)
#' @export
parse_symbols <- function(instruments, nm="exante_id") {
  instruments <- as.data.frame(instruments, nm=nm, stringsAsFactors=F)
  if(nrow(instruments)==0)
    return(instruments)
  id <- instruments[[nm]]
#  print(id)
  instruments <- parse_exante_id(id, instruments)
  instruments 
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
#'     exante_id ric first_notice_day ticker exchange instrument_id month year instrument_class strike
#' ZW.CBOT.H2017 WH7       2017-02-28     ZW     CBOT       ZW.CBOT     3 2017                F     NA
#' ZW.CBOT.K2017 WK7       2017-04-28     ZW     CBOT       ZW.CBOT     5 2017                F     NA
#' ...
#'  > c("VIX.CBOE","PL.NYMEX") %>% parse_symbols() %>% query_symbols(start=now())
#'
#' @export 
query_symbols <- function(instruments = NULL, 
                        start = NULL, 
                        stop = NULL, 
                        where = NULL, 
                        f.prefix = F,
                        fields = list("exante_id", "first_notice_day"), 
                        fields.dt = list("first_notice_day", "first_trading_day", "expiry_date", "last_delivery_day", "last_delivery_day")) {
  # convert to data.frame
  instruments <- parse_symbols(instruments)
  # prepare WHERE
  w <- c(
    instruments[["instrument_id"]] %>% nnmap( ~ .sql.match_id(.x, "instrument_id", f.prefix)), # exante_id = 'XYZ' or exante_id = 'ABC' ...
    start %>% nnmap( ~ paste("first_notice_day", ">=", trunc(as.numeric(.x)*1000))), # start date
    stop %>% nnmap( ~ paste("first_notice_day", "<", trunc(as.numeric(.x)*1000))), # stop date
    where
    ) %>% reduce(sql.and)
  # proceed with SQL
  result <- sql.select("quant_data.smart_symbols", 
             fields = fields,
             where = w,
             order = "first_notice_day")
  # fix datetime columns
  if(nrow(result)>0 && !is.null(fields.dt)) {
    if(!is.null(fields))
      fields.dt<-intersect(fields.dt, fields)
    for(f in fields.dt)
      result[[f]] <- as.datetime(result[[f]])
    # return result
    result <- result %>% parse_symbols
    # add additional fields from instruments
    #browser()
    instruments <- query_instruments(instruments)
    result <- result %>% left_join(instruments %>% select(-exante_id), by="instrument_id")
  }
  return(result)
}

#' query quant_data
#' @examples 
#'   query_instruments("VIX")
#' @export
query_quant_data <- function(x, table, nm, fields = NULL, json_cols = NULL, f.prefix = F, ...) {
  # TODO: as_tibble ?
  w <- .sql.match_id(x, name=nm, f.prefix=f.prefix)
  #result <- tryCatch(
  result <- sql.select(table, fields = fields, where = w, ...)
  if(nrow(result)!=0){
    #warning=function(w){})
    for(col in json_cols) {
      result[[col]] <- result[[col]] %>% map (fromJSON)
    }
  }
  result
}

#' query_instruments
#' @examples 
#'  query_instruments("VIX")
#' @export

query_instruments <- function(instruments = NULL, 
                             fields = c("instrument_id", "currency", "mpi", "commission", "contract_multiplier as multiplier", "active_contracts as active_contract"), 
                             json_cols = c("active_contract"), ...) {
  instruments <- parse_symbols(instruments) # TODO: WHY nm = "instrument_id"
  r <- query_quant_data(instruments$instrument_id, "quant_data.smart_instruments", nm = "instrument_id", fields=fields, json_cols=json_cols, ...)
  # FIXME: patch exante_id == instrument_id so every symbol df has some exante_id column
  r$exante_id <- r[["instrument_id"]]
  return(r)
}


