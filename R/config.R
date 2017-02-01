# misc utils

#' load config from url (yaml file path)
#' note: only file paths implemented
#' @param url file path or http(s) url or postgresql:// url
#' @examples
#'   config <- config.load(url="~/My_config.yaml")
#' @export
cfg.load <- function(url=getOption("ticks.config.url","~/.rticks.yaml"),...) {
  if(file.exists(url)) {
    structure(yaml.load_file(url), class="cfg")
  }else if(!is.null(url)) {
    warning("RTICKS: File ",url," doesn't exist. config() is empty. Use cfg.load(\"~/.rticks.yaml\") to fix.")
    cfg()
  }
}

#' updates config object
#' @examples
#'   cfg.load() %>% update(path=list(myPath="~/myData"))
#' @export
update.cfg <- function(self, ...) modifyList(self, list(...))

#' converts R object into config class
#' @examples 
#'   as.cfg(list(a=1, b=2))
#' @export
as.cfg  <- function(value) structure(value, class="cfg") 

#' convert symbols dataframe to list of config objects
#' @export
as.cfglist <- function(df, id) listby(df, id) %>% map(~ as.cfg(.x) ) %>% as.cfg()


#' creates configuration
#' @examples 
#'   cfg(a=1, b=2)
#' @export
cfg <- function(...) structure(list(...), class="cfg")

#' prints config in YAML format
#' @examples 
#'   cfg(a=1, b=2) %>% print()
#' @export
print.cfg  <- function(self,...) cat(as.yaml(self))

#' sets cfg as global
#' @examples 
#'    cfg.set(cfg.load(url="~/mycfg.yaml"))
#' @export
cfg.set <- function(cfg) {
  options(ticks.config = cfg)
}

#' gets global cfg and optionally returns additional transient fields
#' @examples 
#'   cfg.get(some_addition=1)
#' @export
cfg.get <- function(...) getOption("ticks.config", cfg()) %>% update(...)

#' gets item from global config
#' @examples 
#'   config(db)
#'   config(db, user)
#' @export
config <- function(sect, key, global=cfg.get()) {
  #cat("[",substitute(sect),"]")
  if(missing(sect))
    return(global)
  val <- global[[deparse(substitute(sect))]]
  if(!missing(key))
    val <- val[[deparse(substitute(key))]]
  if(is.list(val))
    as.cfg(val)
  else
    val
}
