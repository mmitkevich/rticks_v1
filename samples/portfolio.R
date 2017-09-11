library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(xts)
library(rticks)
library(ggplot2)
library(grid)



wf_portfolio <- function(R, 
  obj_type = "risk",
  obj_name = "ES",
  
  optimize_method = "quadprog", #"DEoptim"
  trace = TRUE,
  search_size = 1000,
  rebalance_on = "months",
  training_period = 180,
  rolling_window = 180,
  rp = NULL,
  max.w = 0.5,
  max.untraded = 0.7) {
  browser()    
  ep.i <- endpoints(R, on = rebalance_on)[which(endpoints(R, 
                                                        on = rebalance_on) >= training_period)]

  cluster <- makeCluster(1)
  registerDoParallel(cluster)
  set.seed(2100)

  obj_name = "ES"
  obj_arguments = list(p=0.95,clean="boudt")

  start_t <- Sys.time()

  #out_list <- foreach::foreach(ep = iterators::iter(ep.i),                            .errorhandling = "pass", .packages = "PortfolioAnalytics") %do%  # %dopar%
  for(ep in ep.i)
  {
                              browser()
                               ret <- R[(ifelse(ep - rolling_window >= 1, ep - rolling_window, 1)):ep, ]
                               nulls <- apply(X = ret, MARGIN = 2, FUN = function(arg) length(which(arg==0))/length(arg))
                               ret_to_test <- ret[, nulls < max.untraded] 
                               portfolio_ <- portfolio.spec(assets = names(ret_to_test)) #, weight_seq = generatesequence(min= 0.02, by = 0.02)
                               portfolio_ <- add.constraint(portfolio = portfolio_, type = "box", min = 0.02, max = max.w)
                               portfolio_ <- add.constraint(portfolio= portfolio_, type="leverage", min_sum=1, max_sum=1)
                               #portfolio_ <-  add.objective(portfolio=portfolio_, type="risk", name="CDD")
                               #portfolio_ <- add.objective(portfolio=portfolio_, type="risk", name="CVaR", arguments=list(p=0.95, clean="boudt"))
                               portfolio_ <- add.objective(portfolio=portfolio_, type=obj_type, name=obj_name, arguments=obj_arguments)
                               #portfolio_ <- add.objective(portfolio=portfolio_, type="return", name="mean")
                               optimize.portfolio(ret_to_test, portfolio = portfolio_, 
                                                  optimize_method = optimize_method, search_size = search_size, 
                                                  trace = trace, rp = rp, parallel = FALSE)
                               
  }
  names(out_list) <- index(R[ep.i])
  end_t <- Sys.time()
  elapsed_time <- end_t - start_t
  wlog("ELAPSED ", elapsed_time)
  stopImplicitCluster()
  name = paste("obj~",obj_name, "max.w~", max.w,", search~",search_size,", roll.wind~",rolling_window,", max.untrad~",max.untraded, sep=".")
  weights = out_list %>% map2(names(out_list), ~ .x$weights %>% as.list %>% as.data.frame) %>% bind_rows() %>% setNames(names(out_list))
  returns = Return.portfolio(R = R, weights = weights)
  list(name=name, weights=weights, returns=returns)
}

#chart.CumReturns(returns.all.xts, wealth.index = T, legend.loc = "topleft", main = "Cumulative Returns")
#chart.CumReturns(returns.all.xts[,c(1,2,3,4,5,6)], wealth.index = T, legend.loc = "topleft", main = "Cumulative Returns 1/3")
#chart.CumReturns(returns.all.xts[,c(7,8,9,10,11,12)], wealth.index = T, legend.loc = "topleft", main = "Cumulative Returns 2/3")
#chart.CumReturns(returns.all.xts[,c(13,14,15)], wealth.index = T, legend.loc = "topleft", main = "Cumulative Returns 3/3")
###########################################################################

poutdir <- paste0("~/results/portfolio/",run_name_today(),"/")
mkdirs(poutdir)
bt<- yaml.load_file(config(path)$gridPath)
indir <- paste0(bt$config$outdir, run_name)
run_name <- bt_list_runs() %>% tail(1)
#run_name <- "20170623"
r <- bt_load_results(name = run_name)
eqData <- r$metrics_file %>% map( ~ read.csv(file=paste0(indir,"/",.), stringsAsFactors=F)) %>% setNames(r$metrics_file %>% str_left(nchar(".metrics.csv")))
#R <- eqData %>% map(function(m) xts(m$rtn, order.by = as.Date(m$datetime, format="%Y-%m-%d %H:%M:%S"))) %>% reduce(cbind) %>% setNames(names(eqData)) %>% na.fill(as.numeric(0)) %>% as.xts()

eqData.xts <- lapply(eqData, function(arg) {xts(arg$rtn, order.by = as.Date(arg$datetime, format="%Y-%m-%d %H:%M:%S"))})
returns.all.xts <- do.call("cbind",  eqData.xts)
R <- na.fill(object = returns.all.xts, fill = as.numeric(0))

r <- R %>% wf_portfolio()

r$weights %>% write.csv(file=paste0(poutdir, "weights.csv"))
r$returns %>% write.csv(file=paste0(poutdir, "portfolio.csv"))
ggsave(paste0(poutdir, "preturns-", r$name, ".png"))
ggsave(paste0(poutdir, "pweights-", r$name, ".png"))
#table.AnnualizedReturns(optimized.return)
#table.DownsideRisk(optimized.return)
r$eq_weights <- as.equal.weights(r$weights)
r$eqw_returns <- Return.portfolio(R = returns.all.xts, weights = r$eq_weights)
r

#charts.PerformanceSummary(r$returns, main = r$name, wealth.index = T)
#chart.StackedBar(r$weights, main = r$name)
#charts.PerformanceSummary(r$eqw_returns, main = r$name, wealth.index = T)
#chart.StackedBar(r$eq_weights, main = paste0("eqw-", r$name))

