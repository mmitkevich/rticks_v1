library(rticks)
library(ggplot2)
library(grid)

options(debug=F)
#options(error = recover)
cfg.reload()
parinit(0)
bt <- config.gamma("samples/grid.yaml")
bt <- bt %>% run_all.gamma(
  enabled = c("C", "CL", "CC"), 
  run_name = run_name_today("KL-WF-test"),
  IIS = c(5, 5, 10, 10, 10), 
  OOS = c(1, 5, 1, 5, 10)
)
parstop()
last_plot()
bt_plot(bt$runs[[1]])
#