.onLoad <- function(libname, pkgname){
  url <- getOption("ticks.config.url", "~/.rticks.yaml")
  cat("cfg.load(",url,")\n")
  cfg.set(cfg.load(url))
}