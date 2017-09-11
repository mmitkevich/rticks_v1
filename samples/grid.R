library(rticks)
library(ggplot2)
library(grid)

options(debug=F)
#options(error = recover)
cfg.reload()
parinit(0)
bt <- config.gamma("samples/grid_po.yaml")
bt <- bt %>% run_all.gamma(
  enabled = c("LH"), 
  run_name = run_name_today("LH-test"), 
  keep_data = F, 
  IIS = c(1, 5, 10, 100))
parstop()
last_plot()
bt_plot(bt$runs[[1]])
