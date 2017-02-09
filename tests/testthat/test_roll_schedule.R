library(rticks)
library(testthat)

cat("\n\n\n***** test_roll_schedule.R ****\n\n\n")

#source("~/r-tools/schedule/roll_schedule.R")

test_that("get_futures_contract_number", expect_equal(get_futures_contract_number("GOLD.FORTS.H2016"),3))

test_that(".get_futures_contract_letter", expect_equal(get_futures_contract_letter("GOLD.FORTS.H2016"), "H"))


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


schedule <- roll_schedule(c("GOLD.FORTS", "PL.NYMEX", "FOAT.EUREX"),  
                          active_contract = list(GOLD.FORTS = c(3, 6, 9, 12), PL.NYMEX = c(1, 4, 7, 10), FOAT.EUREX = c(3,6,9,12)),
                          max_active_contract = 10, 
                          start = as_datetime("2015-01-01"), 
                          stop = as_datetime("2017-01-01"))

test_that("nrow(schedule)", 
          expect_equal(nrow(schedule), 819))
test_that("unique(schedule$instrument_id)", 
          expect_equal(as.character(unique(schedule$instrument_id)), c("GOLD.FORTS", "PL.NYMEX", "FOAT.EUREX")))
test_that("unique(schedule$active_contract)", 
          expect_equal(unique(schedule$active_contract), c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "0")))
test_that("schedule$exante_id[1]", 
          expect_equal(schedule$exante_id[1], "GOLD.FORTS.H2010"))
test_that("schedule$exante_id[20]", 
          expect_equal(schedule$exante_id[20], "PL.NYMEX.J2012"))

