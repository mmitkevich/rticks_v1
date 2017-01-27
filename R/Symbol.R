#' fetch something...
#' @export
fetch <- function(x,...) UseMethod("fetch")

#' store something...
#' @export
store <- function(x,...) UseMethod("store")

#' specify symbol
#' @examples 
#'   s(VXM2016.CBOE)
#'      VXM2016.CBOE
#' @export
s <- function(id,...) structure(list(exante_id=id, ...), class="symbol") 

#' print symbol
#' @examples 
#'   s(VIX) %>% print()
#' @export
print.symbol <- function(self, ...) cat(as.yaml(self), "\n")

#' connect to SQL database
#' @examples 
#'   con <- sql.connect()
#' @export
sql.connect <- function(db=config(db)) {
  con <- dbConnect(db$dbDriver, dbname=db$dbname, host=db$host, port=db$port, user=db$user, password=db$password)
  con
}

#' paste list
#' @export
paste.list <- function(lst, sep="") {
  do.call(paste, modifyList(lst,list(sep=sep)))  
} 

#' execute SQL select from SQL database
#' @examples 
#'   sql.select("quant_data.symbols")
#' @export
sql.select <- function(from, fields=NULL, where=NULL, order=NULL, limit=1000, con=NULL, db=config(db), no_cache=TRUE) {
  if(!no_cache) {
    
  }
  if(is.null(fields))
    fields <- list("*")
  have.con = !is.null(con)
  if(!have.con)
    con = sql.connect(db=db)
  where <- ifelse(is.null(where), "", paste("WHERE", where))
  limit <- ifelse(is.null(limit), "", paste("LIMIT", limit))
  order <- ifelse(is.null(order), "", paste("ORDER BY", order))
  sql <- paste("SELECT", paste.list(fields, sep=","), "FROM", from, where, order, limit)
  if(getOption("debug",F))
    cat("SQL: ", sql, "\n")
  result <- dbGetQuery(con, sql)
  if(!have.con)
    dbDisconnect(con)
  return(result)
}

sql.quote <- function(value) paste0("'",value,"'")


#' applies function to value or keeps NULL value
#' @examples
#'  nnmap(NULL,function(x) 2*x) 
#'    NULL
#'  nnmap(1, function(x) 2*x)
#'    2
#' @export
nnmap <- function(x, f,...) {
  if(is.null(x))
    return(NULL)
  else
    return(f(x,...))
}  

#' fuzzy SQL equals
#' @export
sql.eq_or_like <- function(name, value) paste(name, ifelse(grepl("%",value),"LIKE","="), sql.quote(value))

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
  paste(x,"AND",y)  
}

#' @export
ifelsenull<-function(x,f,true,fals) {
  if(is.null(x))
    return(NULL)
  if(f(x))
    return(true);
  return(false)
}
#' query symbols from SQL database
#' @param start               # min(first notice day)
#' @param end                 # max(first notice day)
#' @examples                  
#'   query_symbols()          # returns everything (specify limit=1000000 to return more than 1000 elements)
#'   query_symbols("VI")     # returns pattern matching VIX.CBOE, VIZ.CME, etc
#'   query_symbols(stop = now() + months(1)) # only expiring in 1 month
#'   query_symbols("ZW", stop = as_dt(18)) # all 2018-expired ZW
#'   query_symbols("VIX", start = as_dt(16), stop = as_dt(18)) # VIX from 2016 till 2018
#' @export 
query_symbols <- function(x=NULL, start=lubridate::now("GMT"), stop=NULL, where=NULL, 
                          f.symlist=F, 
                          f.prefix=T,
                          fields=list("exante_id", "ric", "fut_notice_first"), 
                          fields.dt=list("fut_notice_first", "fut_first_trade_dt", "last_tradeable_dt", "fut_dlv_dt_first", "fut_dlv_dt_last")) {
  w <- Reduce(sql.and, c(
    nnmap(x, function(x) sql.eq_or_like(ifelse(f.prefix, paste0(x,"%"),x), name="exante_id")),
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
  if(f.symlist) {
    result <- result %>% as.symlist("exante_id")
  }
  result
}

#' convert dataframe to list of lists
#' @export
as.rowlist <- function(df, id) {
  setNames(split(df, seq(nrow(df))), df[[id]])
}

#' convert symbols dataframe to list of config objects
#' @export
as.symlist <- function(df, id) as.rowlist(df, id) %>% map(~ as.cfg(.x) ) %>% as.cfg()

#' as_datetime with prefix support
#' @examples 
#'   as.dt("2016")
#'   as.dt(2016)
#'   as.dt(16)
#'   as.dt("2016-01")
#'   as.dt("2016-01-02")
#' @export
as_dt <- function(x) {
  x <- as.character(x)
  if(nchar(x)==2)
    x <- paste0("20", x)
  if(nchar(x)==4)
    x <- paste0(x,"-01")
  if(nchar(x)==7)
    x <- paste0(x,"-01")
  as_datetime(x)
}

schedule <- function(symbols, pattern) {
  
}