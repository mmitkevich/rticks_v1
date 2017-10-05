library(rticks)
library(ggplot2)
library(grid)

options(debug=F)
#options(error = recover)
cfg.reload()
parinit(0)
bt <- config.gamma("samples/grid_test_results.yaml")
bt <- bt %>% run_all.gamma(
  enabled = c("C"), 
  run_name = run_name_today("WF-ALL-rpnl")
  #IIS = c(30, 60, 90), 
  #OOS = c(1,   5, 30)
)
parstop()
last_plot()
bt_plot(bt$runs[[1]])
#