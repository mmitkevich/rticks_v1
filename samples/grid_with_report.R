library(rticks)
library(ggplot2)
library(grid)
library(yaml)

options(debug=T)
cfg.reload()

r <- run_all.gamma(enabled=c("C","CC"),
                   bt="samples/grid_bt.yaml",
                   run_name = run_name_today("test"),
                   parallel=T)

path  <-  r[[1]]$config$log_path
path <- paste(unlist(strsplit(path, '/'))[1:(length(unlist(strsplit(path, '/'))) - 3)], collapse = "/")
print(paste0(path,'/'))

root_program_dir <- "~/"
global_config <- yaml.load_file(paste0(root_program_dir, "Exante_R_config.yaml"))
results_path <- paste0(path,'/')
results_csv <- read.csv(paste0(results_path, "results.csv"))

for (instrument in unique(results_csv$name)) {
  results  <- results_csv[which(results_csv$name == instrument), ]
  instrument <- substr(results$symbol[1],0,tail(gregexpr(pattern ='\\.',results$symbol[1])[[1]], 1) - 1)
  limit.buy <- results$limit.buy[1]
  stop.buy <- results$stop.buy[1]
  risk.buy <- results$risk.buy[1]
  ticker <- results$name[1]
  mpi <- results$mpi[1]
  multiplier <- results$multiplier[1]
  filename <- paste0(results_path, instrument, '_', Sys.Date())
  time_from <- read.csv(paste0(results_path,ticker,"/",results$metrics_file[1]))$datetime[1]
  time_to <- tail(read.csv(paste0(results_path,ticker,"/",results$metrics_file[1]))$datetime,1)
  
  rmarkdown::render(paste0(global_config$path$toolsfolderPath,'markdown_report/','report.Rmd'), output_file = paste0(filename, '.pdf'), params = list(
    results = results,
    results_path = results_path,
    instrument = instrument,
    time_from = time_from,
    time_to = time_to,
    limit.buy = limit.buy,
    stop.buy = stop.buy,
    risk.buy = risk.buy,
    ticker = ticker,
    mpi = mpi,
    multiplier = multiplier,
    active_contracts = max(results$active_contract),
    split_period = "month"
  ))
}