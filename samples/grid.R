library(rticks)
library(ggplot2)
library(grid)

options(debug=F)
#options(error = recover)
cfg.reload()
parinit(8)
bt <- config.gamma("samples/grid_bt.yaml")
MINS_PER_DAY=7*60*60
bt <- bt %>% run_all.gamma(enabled=c("BRUB"), run_name = run_name_today("BRUB-min-test-WF"), keep_data = F, IIS = c(3*MINS_PER_DAY))
parstop()
last_plot()
