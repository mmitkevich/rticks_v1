library(settings)

fetch <- function(x,...) UseMethod("fetch")
store <- function(x,...) UseMethod("store")

cfg.global(cfg.load())