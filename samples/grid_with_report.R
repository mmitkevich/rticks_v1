library(rticks)
library(ggplot2)
library(grid)
library(yaml)
library(sendmailR)

options(debug=T)
cfg.reload()

r <- run_all.gamma(enabled=c("FSMI"),
                   bt="samples/grid_bt.yaml",
                   run_name = run_name_today("FSMI-test"),
                   parallel=T)

path  <-  r$runs[[1]]$config$log_path
path <- paste(unlist(strsplit(path, '/'))[1:(length(unlist(strsplit(path, '/'))) - 3)], collapse = "/")
print(paste0(path,'/'))

root_program_dir <- "~/"
global_config <- yaml.load_file(paste0(root_program_dir, "Exante_R_config.yaml"))
results_path <- paste0(path,'/')

for (instrument in unique(r$results$name)) {
  results = r$results[which(r$results$name == instrument), ]
  instrument <- substr(results$symbol[1],0,tail(gregexpr(pattern ='\\.',results$symbol[1])[[1]], 1) - 1)
  limit.buy <- results$limit.buy[1]
  stop.buy <- results$stop.buy[1]
  risk.buy <- results$risk.buy[1]
  ticker <- results$name[1]
  mpi <- results$mpi[1]
  multiplier <- results$multiplier[1]
  filename <- paste0(results_path, instrument, '_', Sys.Date())
  time_from <- r$metrics[[1]]$datetime[1]
  time_to <- tail(r$metrics[[1]]$datetime, 1)
  
  rmarkdown::render(paste0(global_config$path$toolsfolderPath,'markdown_report/','report.Rmd'), output_file = paste0(filename, '.pdf'), params = list(
    results = results,
    results_path = results_path,
    instrument = instrument,
    time_from = r$config$start,
    time_to = r$config$stop,
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

if (!is.null(global_config$config$email)) {
  if (exists("url_prefix") & exists("username")) {
    sendmail(from='report@quant1.prod.ghcg.com',to=global_config$config$email, subject = 'Backtest report is ready', 
             msg = paste0('Your backtest report is ready ',url_prefix, username, '/', substr(raw_data_path, 0, tail(gregexpr(pattern = '/', raw_data_path)[[1]],2))) )
  }
}