library(rticks)
library(ggplot2)
library(grid)

options(debug=T)
options(error = recover)
cfg.reload()
parinit(0)
bt <- config.gamma("samples/grid_test.yaml")

bt <- bt %>% run_all.gamma(enabled="PL-GC", run_name = run_name_today("PL-GC-WF"), keep_data = F, IIS = c(1,3,10,20))

parstop()
last_plot()