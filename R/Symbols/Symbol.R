library(lubridate)
library(dplyr)
library(R.utils)

s <- function(...) structure(list(...), class="symbol") 

print.symbol <- function(self, ...) cat(paste(self), "\n")

fetch.symbol <- function(self, start=NULL, stop=lubridate::now("GMT")) {
  db <- cfg.get(db)
  db$dbDriver<-NULL
  symbols.tbl <-  do.call(src_postgres, db) %>% tbl("quant_data.symbols")
}
