#' loads config from url (yaml file path)
#' usage:
#'   config <- config.load(url="~/My_config.yaml")
#' @param url file path or http(s) url or postgresql:// url
#' note: only file paths implemented
cfg.load <- function(url=getOption("ticks.cfg.url", "~/Exante_R_config.yaml"),...) {
  cfg <- structure(yaml.load_file(url), class="cfg")
}

#' updates config object
#' usage:
#'   config <- config.load() %>% update(path=list(myPath="~/myData"))
#'   print(config)
update.cfg <- function(self, ...) modifyList(self, list(...))

#' converts R object into config class
as.cfg  <- function(value) structure(value, class="cfg") 

cfg <- function(...) structure(list(...), class="cfg")

#' prints config in YAML format
print.cfg  <- function(self,...) cat(as.yaml(self))

cfg.set <- function(cfg) {
  options(ticks.config = cfg)
}

cfg.global <- function(...) getOption("ticks.config", cfg()) %>% update(...)

cfg.get <- function(sect, global=cfg.global) {
  val <- global()[[deparse(substitute(sect))]]
  if(is.null(val))
    return(cfg())
  else
    as.cfg(val)
}