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
dt <- function(x) {
  if(is.null(x))
    return(x)
  if(is.POSIXct(x))
    return(x)
  x <- as.character(x)
  if(nchar(x)==2)
    x <- paste0("20", x)
  if(nchar(x)==4)
    x <- paste0(x,"-01")
  if(nchar(x)==7)
    x <- paste0(x,"-01")
  as_datetime(x)
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