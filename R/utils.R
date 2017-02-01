# misc utils

# convert unix timestamp to POSIXct
#' @export
as.datetime = function(datetime) {
  lt<-as.POSIXct(as.numeric(datetime)/1000, origin="1970-01-01")
}

# convert unix timestamp to POSIXct
#' @export
as.date = function(datetime) {
  lt<-as.POSIXct(as.numeric(datetime)/1000, origin="1970-01-01")
}

#' paste list
#' @export
paste.list <- function(lst, sep="") {
  do.call(paste, modifyList(lst,list(sep=sep)))  
} 


#' applies function to value or keeps NULL value
#' @examples
#'  nnmap(NULL,function(x) 2*x) 
#'    NULL
#'  nnmap(1, function(x) 2*x)
#'    2
#' @export
nnmap<-function(.x, ...) {
  if(is.null(.x))
    return(NULL)
  else
    as_function(...)(.x)
}

#' converts c("a","b") to list(a=NULL,b=NULL)
#' @examples 
#' c("GD","PL") %>% nlist %>% map_at("GD", ~ 12) %>% as.cfg
#' @export
nlist <- function(.x, .f=function(x) x) sapply(.x, .f, simplify=F, USE.NAMES=T)

#' convert dataframe to list of lists
#' @export
listby <- function(df, id) {
  setNames(split(df, seq(nrow(df))), df[[id]])
}

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

mutate_rows <- function(.d, .k, .v, .fk, .fv) {
  #col <- .d[[deparse(substitute(.k))]]
  #k<-deparse(substitute(.k))
  #print(f)
  #p<-.d[substitute(.f)]
  #print(p)
  #substitute(.d$.k) <- ifelse(substitute(.f, env=.d), .v, substitute(.d$.k))
  #print(col)
  #return(col)
  #print(substitute(.f))
  #filter_(.d, substitute(.f))$a<-500
  #col[[f]][substitute(.f)] <- substitute(.v)
  #.d
  c <- .d[[deparse(substitute(.k))]]
  p <- .d[[deparse(substitute(.fk))]] == .fv
  c[sla]
  .d
}

