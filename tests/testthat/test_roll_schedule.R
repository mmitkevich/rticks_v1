library(rticks)
library(testthat)

cat("\n\n\n***** test_roll_schedule.R ****\n\n\n")

#source("~/r-tools/schedule/roll_schedule.R")
source("~/rticks/R/roll_schedule_vgo.R")
source("~/rticks/R/clean_chunk.R")

test_that("get_futures_contract_number", expect_equal(get_futures_contract_number("GOLD.FORTS.H2016"),3))

test_that(".get_futures_contract_letter", expect_equal(get_futures_contract_letter("GOLD.FORTS.H2016"), "H"))

####################################################################################################

# TODO test with spreads

####################################################################################################

test_that(".mapping_file_PL.NYMEX.J2016", 
          expect_equal(.mapping_file("PL.NYMEX.J2016", "NYMEX_futures_PL")))

test_that(".mapping_file_VIX.CBOE.J2016", 
          expect_equal(.mapping_file("VIX.CBOE.J2016", "CBOE_futures_VIX")))

test_that(".mapping_file_FOAT.EUREX.J2016", 
          expect_equal(.mapping_file("FOAT.EUREX.J2016", "EUREX_futures_FOAT")))

test_that(".mapping_file_GOLD.FORTS.H2017", 
          expect_equal(.mapping_file("GOLD.FORTS.H2017", "FORTS_futures_all")))

test_that(".mapping_file_PL.TOCOM.V2017", 
          expect_equal(.mapping_file("PL.TOCOM.V2017", "TOCOM_futures_all_except_rubber")))

test_that(".mapping_file_RU.TOCOM.H2017", 
          expect_equal(.mapping_file("RU.TOCOM.H2017", "TOCOM_futures_RU")))

####################################################################################################

# get historical schedule by instrument
startDateTime <- as_datetime("2016-01-01")
endDateTime = as_datetime("2016-03-01")
histTimeSched <- .get_historical_schedule(exanteID = "VIX.CBOE.M2016", start = startDateTime, end = endDateTime, exclude = FALSE)

test_that("hist schedule colnames", 
          expect_equal(colnames(histTimeSched), c("start", "end")))
test_that("schedule start hours - 23",
          expect_equal(hour(histTimeSched$start[date(histTimeSched$start) == "2016-01-10"]), 23))
test_that("schedule start hours - 21",
          expect_equal(hour(histTimeSched$start[date(histTimeSched$start) == "2016-01-11"]), 21))
test_that("schedule start min - 30",
          expect_equal(minute(histTimeSched$start[date(histTimeSched$start) == "2016-01-11"]), 30))
test_that("schedule end hour - 21",
          expect_equal(hour(histTimeSched$end[date(histTimeSched$start) == "2016-01-11"]), 21))
test_that("schedule end min - 15",
          expect_equal(minute(histTimeSched$end[date(histTimeSched$start) == "2016-01-11"]), 15))

####################################################################################################


firstExanteID = "GOLD.FORTS.H2016"
activeMonthPattern = c(3, 6, 9, 10, 11, 12)
max_contract_sequence = 11

r <- .get_act_num_contract_after1(firstExanteID, activeMonthPattern, max_contract_sequence)

test_that(".get_act_num_contract_after1$exanteID[1]", 
          expect_equal(r$exanteID[1], "GOLD.FORTS.H2016"))
test_that(".get_act_num_contract_after1$active_contract[1]", 
          expect_equal(r$activeContractNumber[1], "1"))
test_that(".get_act_num_contract_after1$exanteID[6]", 
          expect_equal(r$exanteID[6], "GOLD.FORTS.Z2016"))
test_that(".get_act_num_contract_after1$active_contract[6]", 
          expect_equal(r$activeContractNumber[6], "6"))

firstExanteID = "PL.NYMEX.N2016"
activeMonthPattern = c(7, 10)
max_contract_sequence = 6

s <- .get_act_num_contract_after1(firstExanteID, activeMonthPattern, max_contract_sequence)

test_that(".get_act_num_contract_after1$exanteID[2]", 
          expect_equal(s$exanteID[2], "PL.NYMEX.V2016"))
test_that(".get_act_num_contract_after1$active_contract[2]", 
          expect_equal(s$activeContractNumber[2], "2"))
test_that("nrows(.get_act_num_contract_after1)", 
          expect_equal(nrow(s), 6))

##################################################################################################
# test function .query_symbols <- function(instruments = NULL, 
instruments <- c("PL.NYMEX", "RTS.FORTS", "FOAT.EUREX")
start <- as_datetime("2014-01-01")
stop <- as_datetime("2017-01-01") 
where <- NULL 
f.prefix <- T
fields <- list("exante_id", "ric", "fut_notice_first")
fields.dt <- list("fut_notice_first", "fut_first_trade_dt", "last_tradeable_dt", "fut_dlv_dt_first", "fut_dlv_dt_last")

symb <- query_symbols(instruments, 
                      start, 
                      stop, 
                      where, 
                      f.prefix,
                      fields,
                      fields.dt)

test_that("unique tickers in symbols", 
          expect_equal(unique(symb$ticker), c("RTS", "FOAT", "PL")))
test_that("unique months in symbols", 
          expect_equal(unique(symb$month), c(3,  4,  6,  7,  9, 10, 12,  1)))
symb_PL <- symb %>% filter(instrument_id == "PL.NYMEX")
test_that("test for PL symbols", 
          expect_equal(symb_PL$ric, c("PLJ4","PLN4","PLV4","PLF5","PLJ5","PLN5","PLV5","PLF6","PLJ6","PLN6","PLV6","PLF7")))


##################################################################################################

instruments <- c("GOLD.FORTS", "PL.NYMEX", "FOAT.EUREX")
active_contract <- list(GOLD.FORTS = c(3, 6, 9, 12), PL.NYMEX = c(1, 4, 7, 10), FOAT.EUREX = c(3,6,9,12))
max_active_contract <- 10
start <- as_datetime("2015-01-01")
stop <- as_datetime("2017-01-01")
nm <- "instrument_id"

schedule <- roll_schedule_vgo(instruments,  
                              active_contract,
                              max_active_contract, 
                              start, 
                              stop,
                              nm)

test_that("nrow(schedule)", 
          expect_equal(nrow(schedule), 819))
test_that("unique(schedule$instrument_id)", 
          expect_equal(as.character(unique(schedule$exanteIDpat)), c("GOLD.FORTS", "PL.NYMEX", "FOAT.EUREX")))
test_that("unique(schedule$active_contract)", 
          expect_equal(unique(schedule$activeContractNumber ), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "0")))
test_that("schedule$exante_id[1]", 
          expect_equal(schedule$exanteID [1], "GOLD.FORTS.H2010"))
test_that("schedule$exante_id[20]", 
          expect_equal(schedule$exanteID [20], "PL.NYMEX.J2012"))

