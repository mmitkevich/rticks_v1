library(rticks)
library(ggplot2)
library(grid)

options(debug=F, cluster=4)
#options(error = recover)
cfg.reload()
bt <- config.gamma("samples/grid.yaml")
bt <- bt %>% run_all.gamma(
#  enabled = c("C", "CL", "CC"), 
  run_name = run_name_today("KL-WF-test")
)
last_plot()
bt_plot(bt$runs[[1]])
