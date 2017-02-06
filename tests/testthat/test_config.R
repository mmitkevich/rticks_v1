cat("\n\n\n***** test_config.R ****\n\n\n")

options(rticks.config.url="~/Exante_R_config.yaml")

library(testthat)
library(rticks)

# load another config
my_cfg <- cfg.load("~/Exante_R_config.yaml")

# set config as default
cfg.set(my_cfg)

# print config (which was automatically loaded)
#print(config())

# print config section
print(config(db))

# print config key 
print(config(db, user))

#test_that("config(db,dbname)=='quant'", {
#  expect_equal(config(db, dbname), "quant")
#})