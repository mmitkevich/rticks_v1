library(rticks)
library(testthat)

cat("\n\n\n***** test_roll_schedule.R ****\n\n\n")

#source("~/r-tools/schedule/roll_schedule.R")

test_that("get_futures_contract_number", expect_equal(get_futures_contract_number("GOLD.FORTS.H2016"), 3))

test_that(".get_futures_contract_letter", expect_equal(get_futures_contract_letter("GOLD.FORTS.H2016"), "H"))


firstExanteID = "GOLD.FORTS.H2016"
activeMonthPattern = c(3, 6, 9, 10, 11, 12)
max_contract_sequence = 11

r <- .get_act_num_contract_after1(firstExanteID, activeMonthPattern, max_contract_sequence)

test_that(".get_act_num_contract_after1$exanteID[1]", 
          expect_equal(r$exanteID[1], "GOLD.FORTS.H2016"))
test_that(".get_act_num_contract_after1$active_contract[1]", 
          expect_equal(r$activeContractNumber[1], "1"))


firstExanteID = "PL.NYMEX.N2016"
activeMonthPattern = c(7, 10)
max_contract_sequence = 6

r <- .get_act_num_contract_after1(firstExanteID, activeMonthPattern, max_contract_sequence)

# TODO: test_that

r <- roll_schedule(c("GOLD.FORTS", "PL.NYMEX", "FOAT.EUREX"),  
              active_contract = list(GOLD.FORTS = c(3, 6, 9, 12), PL.NYMEX = c(1, 4, 7, 10), FOAT.EUREX = c(3,6,9,12)),
              max_active_contract = 10, 
              start = as_datetime("2015-01-01"), 
              stop = as_datetime("2017-01-01"))

# TODO: test_that

#############################################################################################
sched <- roll_schedule("PL.NYMEX",  
                       active_contract = list(PL.NYMEX = c(1, 4, 7, 10)),
                       max_active_contract = 3, 
                       start = as_datetime("2015-01-01"), 
                       stop = as_datetime("2017-01-01"))

#active.from <- 2
#active.to <- 1

#getQuotes
#filtering

#query_symbols()
#query_instruments()
#query_quant_data()

####################
#query_schedule()
####################

