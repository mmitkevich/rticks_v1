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

#' execute SQL select from SQL database
#' @examples 
#'   sql.select("quant_data.symbols")
#' @export
sql.select <- function(from, fields="*", where=NULL, limit=1000, con=NULL, db=config(db), no_cache=TRUE) {
  if(!no_cache) {
    
  }
  have.con = !is.null(con)
  if(!have.con)
    con = sql.connect(db=db)
  where <- ifelse(is.null(where), "", paste("WHERE", where))
  limit <- ifelse(is.null(limit), "", paste("LIMIT", limit))
  sql <- paste("SELECT", paste(fields, sep=","), "FROM", from, where, limit)
  if(getOption("debug",F))
    cat("SQL: ", sql, "\n")
  result <- dbGetQuery(con, sql)
  if(!have.con)
    dbDisconnect(con)
  return(result)
}

sql.quote <- function(value) paste0("'",value,"'")
nnmap <- function(x, f,...) {
  if(is.null(x))
    return(NULL)
  else
    return(f(x,...))
}  

sql.eq_or_like <- function(name, value) paste(name, ifelse(grepl("%",value),"LIKE","="), sql.quote(value))

#' fetch symbol from SQL database
#' @examples
#'   fetch(s("VIX.CBOE.M2016")) # will fetch all fields from the symbol database
#' @export 
fetch.symbol <- function(self, start=NULL, stop=lubridate::now("GMT"), ...) {
  query_symbols(self,start,stop,...)
}

#' query symbols from SQL database
#' @examples 
#'   query_symbols()          # returns everything (specify limit=1000000 to return more than 1000 elements)
#'   query_symbols("VI%")     # returns pattern matching VIX.CBOE, VIZ.CME, etc
#' @export 
query_symbols <- function(x, start=NULL, stop=lubridate::now("GMT"), ...) {
  sql.select("quant_data.symbols", 
             where = nnmap(x, 
                           f = Curry(sql.eq_or_like,name="exante_id")), 
             ...)
}