.onLoad <- function(libname, pkgname){
  url <- getOption("ticks.cfg.url", "~/.rticks.yaml")
  cat("cfg.load(",url,")\n")
  cfg.set(cfg.load(url))
}