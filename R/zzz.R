.onLoad <- function(libname, pkgname){
  url <- getOption("rticks.config.url", "~/.rticks.yaml")
  cat(0, "cfg.load(", url,")")
  cfg.set(cfg.load(url))
}