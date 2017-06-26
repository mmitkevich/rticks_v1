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
  do.call(paste, modifyList(as.list(lst),list(sep=sep)))  
} 

#' return cached attribute, or set it if not found 
#' 
#' @export
cached_attr <- function(x, nm, value) {
  val <- attr(x, nm)
  if(!is.null(val))
    return(val)
  attr(x, nm) <- value
  return(value)
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
list_by <- function(df, id, .f=identity) {
  setNames(split(df, seq(nrow(df))) %>% map(~ as_function(.f)(.x)), df[[id]])
}

#' as_datetime with prefix support
#' @examples 
#'   dt("2016")
#'   dt(2016)
#'   dt(16)
#'   dt("2016-01")
#'   dt("2016-01-02")
#' @export
dt <- function(x,y=NULL,z=NULL) {
  if(is.null(x))
    return(x)
  if(is.POSIXct(x))
    return(x)
  x <- as.character(x)
  if(nchar(x)==2)
    x <- paste0("20", x)
  if(nchar(x)==4)
    x <- paste0(x,"-", ifnull(y,"01",as.character(y)))
  if(nchar(x)==7)
    x <- paste0(x,"-", ifnull(z,"01",as.character(z)))
  r<-as_datetime(x)
}

#' grouped df -> list conversion
#' 
#' @export
grouped_df_as_list <- function(.d) (.d %>% do(.out=(.)) %>% select(.out)) [[1]]


#' return f(x) 
#' @export
ifnull <- function(x, default, modified=x) {
  if(is.null(x))
    return(default)
  return(modified)
}

#' replace NA with something
#' 
#' @export
na_replace <- function(x, default = 0) {
  x[is.na(x)] <- default
  return(x)
}

#' LOG levels
#' 
#' @export
LOG = list(TRACE=0, DEBUG=1, INFO=2, WARN=3, ERROR=4, CRITICAL=5, OFF=6)

#' dlog
#' 
#' @export
dlog <- function(...) {
  spd_log(LOG$DEBUG, as.character(c(...)))
}

#' ilog
#' 
#' @export
ilog <- function(...) {
  spd_log(LOG$INFO, as.character(c(...)))
}

#' wlog
#' 
#' @export
wlog <- function(...) {
  spd_log(LOG$WARN, as.character(c(...)))
}

#' 
#' 
#' @export
parse_periods <- function(config, names) {
  for(n in names) {
    if(n %in% names(config))
      config[[n]] <- ifnull(config[[n]], NULL, as.numeric(as.period(config[[n]])))
  }
  config
}

#' 
#' 
#' @export
parse_dates <- function(config, names ){
  for(n in names){ 
    if(n %in% names(config))
      config[[n]] <- ifnull(config[[n]], NULL, as_datetime(config[[n]]))
  }
  config
}
