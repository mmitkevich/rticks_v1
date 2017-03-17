parse_exante_id <- function(id, instruments=NULL) {
  if(is.null(instruments))
    instruments = data.frame(exante_id=id)
  q <- strsplit(id, "\\.")
  instruments$ticker <- q %>% map_chr(~ .x[1])
  instruments$exchange <- q %>% map_chr(~ .x[2])
  future_part <- q %>% map_chr(~ .x[3])
  #print(future_part)
  instruments$month <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), substr(future_part, 4, 4), substr(future_part, 1, 1))
  instruments$month <- match(instruments$month, contract_month_letter)
  #ifelse(is.na(future_part), NA, which.max(contract_month_letter==substr(future_part,1,1)))
  instruments$year <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), as.numeric(substr(future_part,5,8)), as.numeric(substr(future_part,2,5)))
  # month2 and year2 only for standalone spreads
  instruments$month2 <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), substr(future_part, 10, 10), NA)
  instruments$month2 <- match(instruments$month2, contract_month_letter)
  instruments$year2 <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), as.numeric(substr(future_part,11,14)), as.numeric(substr(future_part,2,5)))
  instruments$instrument_id <- ifelse(substr(future_part, 0, 2) %in% c("RS", "CS"), 
                                      paste0(instruments$ticker, ".", instruments$exchange, ".", substr(future_part, 0, 2), (as.numeric(instruments$year2) - as.numeric(instruments$year))*12 + (as.numeric(instruments$month2) - as.numeric(instruments$month)), "M"),
                                      ifelse(is.na(instruments$exchange), instruments$ticker, paste0(instruments$ticker,".",instruments$exchange)))
  
  option_part <- q %>% map(~ .x[4])
  option_type <- substr(option_part,1,1)
  instruments$instrument_class = ifelse(substr(future_part, 0, 2) == "CS", "CS",
                                        ifelse(substr(future_part, 0, 2) == "RS", "RS",
                                               ifelse(!is.na(option_type), option_type,
                                                      ifelse(!is.na(instruments$month), "F", "S"))))
  instruments$strike <- as.numeric(gsub("_","\\.",substr(option_part, 2, nchar(option_part))))
  instruments
}


id <- c("GE.CME.CS/H2019-H2020", "VIX.CBOE.RS/F2015-G2015", "GE.CME.K2015", "ZB.CBOT.Z2012")
id <- c("GE.CME.K2015", "ZB.CBOT.Z2012")

res <- parse_exante_id(id)

