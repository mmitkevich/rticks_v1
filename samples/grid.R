library(rticks)
library(ggplot2)
library(grid)

options(debug=T)
cfg.reload()

r <- run_all.gamma(enabled = "BRUB", run_name=run_name_today(), parallel=T)

last_plot()