library(testthat)
library(rticks)

options(ticks.config.url="~/Exante_R_config.yaml")
library(rticks)

# print config (which was automatically loaded)
print(config())

# print config section
print(config(db))

# print config key 
print(config(db, user))


# load another config
my_cfg <- cfg.load("~/.rticks.yaml")

# set config as default
cfg.set(my_cfg)




test_that("config(db,dbname)=='quant'", {
  expect_equal(config(db, dbname), "quant")
})