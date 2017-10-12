library(rticks)
library(ggplot2)
library(grid)

options(debug=F, cluster=4)
#options(error = recover)
cfg.reload()
#parinit(20)
bt <- config.gamma("samples/grid_bt.yaml")
bt <- bt %>% run_all.gamma(
#  enabled = c("CL"), 
  run_name = run_name_today("WF-ALL-max")
  #IIS = c(30, 60, 90), 
  #OOS = c(1,   5, 30)
)
#parstop()
last_plot()
bt_plot(bt$runs[[1]])
