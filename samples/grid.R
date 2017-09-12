library(rticks)
library(ggplot2)
library(grid)

options(debug=F)
#options(error = recover)
cfg.reload()
parinit(8)
bt <- config.gamma("samples/grid_bt.yaml")
bt <- bt %>% run_all.gamma(
#  enabled = c("LH","C","CC"), 
  run_name = run_name_today("WF-ALL-tmp"), 
  IIS = c(1, 5, 10, 30, 60, 90))
parstop()
last_plot()
bt_plot(bt$runs[[1]])
