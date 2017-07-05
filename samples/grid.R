library(rticks)
library(ggplot2)
library(grid)

options(debug=T)
cfg.reload()

r <- run_all.gamma(enabled="W-SB", bt="samples/grid.yaml")

last_plot()