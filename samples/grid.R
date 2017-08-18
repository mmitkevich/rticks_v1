library(rticks)
library(ggplot2)
library(grid)

options(debug=T)
options(error = recover)

cfg.reload()

parinit(0)

bt <- config.gamma("samples/grid_bt.yaml") %>%   run_all.gamma(enabled="CL", run_name = run_name_today("CL-WF"), keep_data = F,IIS = c(3))

#sig <- bt$forward[[1]] %>% select(datetime, spread)
#all_params <- bt$results %>% group_by(symbol) %>% filter(row_number()==1) %>% select(-weight)
#par <- all_params[1,] %>% as_data_frame()
#par$symbol<-str_left(par$symbol,2)[[1]]
#wfrun <- par %>% backtest(stparams=par, "gamma", start=bt$config$start, stop=bt$config$stop, config=bt$config) 

run_name <- "20170809/092840-CL-WF-YZNHU"
bt <- bt %>% walk_forward.gamma(run_name = run_name, IIS = c(3))


parstop()
last_plot()