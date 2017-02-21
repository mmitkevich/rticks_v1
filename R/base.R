#library(yaml)
#library(lubridate)
#library(dplyr)
#library(R.utils)
#library(yaml)
#library(RPostgreSQL)
#library(DBI)
#library(functional)

# methods declaration

#' fetch something...
#' @export
fetch <- function(x,...) UseMethod("fetch")

#' store something...
#' @export
store <- function(x,...) UseMethod("store")

#' @importFrom Rcpp evalCpp
#' @useDynLib rticks