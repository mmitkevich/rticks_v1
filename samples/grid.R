library(rticks)
library(ggplot2)
library(grid)

options(debug=T)
cfg.reload()

r <- run_all.gamma(enabled=c("C","CC"), bt="samples/grid_bt.yaml", run_name = run_name_today("comment"))

last_plot()