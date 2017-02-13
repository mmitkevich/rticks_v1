
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
sql.select <- function(from, fields=NULL, where=NULL, order=NULL, limit=NULL, con=NULL, db=config(db), no_cache=TRUE) {
  if(!no_cache) {
    
  }
  if(is.null(fields))
    fields <- c("*")
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

sql.quote <- function(.x) paste0("'",.x,"'")


#' fuzzy SQL equals
#' @export
sql.eq_or_like <- function(.x, name) paste(name, ifelse(grepl("%",.x),"LIKE","="), sql.quote(.x))


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


#'
#' @export
.sql.match_id <- function(x, name, f.prefix=T) {
  #nnmap(x, function(x) sql.eq_or_like(ifelse(f.prefix, paste0(x,"%"),x), name=name))
  Reduce(sql.or, Map(function(x) sql.eq_or_like(ifelse(f.prefix, paste0(x,"%"),x), name=name), x))
}
