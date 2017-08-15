library(rticks)
library(ggplot2)
library(grid)

options(debug=T)
options(error = recover)

cfg.reload()

parinit(8)

r <- run_all.gamma(enabled="CL",
                   bt = "samples/grid_bt.yaml",
                   run_name = run_name_today("CL-WF")
                   #keep_data = F,
                   #IIS = c(1, 3, 5, 10, 50, 100, 200)
                   )
# run_name <- "20170809/092840-CL-WF-YZNHU"
# r<- walk_forward.gamma(bt="samples/grid_bt.yaml", run_name = run_name, IIS = c(3), IIS.plot=c(), best_interval = 1.0)

parstop()

last_plot()