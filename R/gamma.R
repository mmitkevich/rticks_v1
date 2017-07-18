#' add metrics
#' 
#' @export
metrics.gamma <- function(env, no_commission=F, currency=NULL) {
  #params = attr(perfs, "params")
  #browser()
  qtys <- env$perfs %>% spread(metric, value) %>% as_data_frame()
  if(has_name(qtys, "commission"))
    qtys <- qtys %>% select(-commission)
  qtys <- qtys %>% inner_join(env$params %>% select(symbol, spread, multiplier, commission), by="symbol")
  qtys <- qtys %>% mutate(
    rpnl = pmin(qty_buy, qty_sell) * spread * multiplier)
  
  if(!no_commission)
    qtys <- qtys %>% mutate(
      rpnl = rpnl - commission, 
      commission = commission*(qty_buy+qty_sell),
      pnl = pnl - commission,
      pnl_high = pnl_high - commission,
      pnl_low = pnl_low - commission)
  else
    qtys <- qtys %>% mutate(commission = 0)
  
  #
  # DRISK = (PRICE-STOP)*(PRICE-RISK)/2 + POS*(PRICE-RISK)
  # PNL = GRID.PNL(PRICE TRAJECTORY IN DAY) + POS*DELTA(PRICE)
  #
  #    GRID.PNL.ZERO(PRICE TRAJECTORY IN DAY) + POS*DELTA(PRICE)
  #  -----------------------------------------------------------
  #     ( 0.5*(PRICE-STOP)  + POS) * (PRICE-RISK)

  p <- env$params
    qtys <- qtys %>% mutate(
    #drisk = (((pmin(bid, p$limit.buy)-pmin(p$stop.buy, bid, na.rm=T))/p$mpi+1)*((pmin(bid,p$limit.buy)-pmin(p$stop.buy,bid, na.rm=T))/2*p$gamma.buy+(p$stop.buy-p$risk.buy))+pos*(price-pmin(p$risk.buy,price)))*p$multiplier,
    drisk = (
              ((pmin(na_replace(bid,p$stop.buy), p$limit.buy)-pmin(p$stop.buy, bid, na.rm=T))/p$mpi+1) # ((price-stop)/mpi + 1)
                  * (
                      (pmin(na_replace(bid,p$stop.buy),p$limit.buy)-pmin(p$stop.buy,bid, na.rm=T))/2 * p$gamma.buy  # (price  - stop)
                      + (p$stop.buy-p$risk.buy) #  (stop-risk)
                    ) 
              + pos * (price-pmin(p$risk.buy,price))) # pos*(price-risk)
          * p$multiplier,
    rtn = (pnl-lag(pnl))/lag(drisk),
    returns = cumsum(na_replace(rtn,0)))
  qtys<- qtys %>% select(-spread,-multiplier) %>% gather(metric, value, -datetime, -symbol)
  qtys
}


#'
#'
#' @export
leg_defaults <- list(symbol=NA, weight=1, power=1, active_contract=0, min_active_contract=0)

#'
#'
#' @export
params_defaults <- list(gamma.buy=1, gamma.sell=1, risk.buy=NA, limit.buy=NA, stop.buy=NA, limit.sell=+Inf, stop.sell=+Inf, spread=1, pos=NA, active_contract=0)

#'
#'
#' @export
run_name_today <- function(comment="",fmt="%Y%m%d/%H%M%S") paste0(strftime(Sys.time(), fmt),"-",comment,"-",paste.list(sample(LETTERS,5)))

#'
#'
#' @export
run_name_unique <- function(fmt="%Y%m%d_%H%M%S") paste0(strftime(Sys.time(), fmt))

all_permutations <- function(params) {
  
}

#'
#'
#' @export
df_chr <-function(df){
  capture.output(print(df))  %>% reduce(function(x,y) paste(x,y,sep="\n"))
}

#'
#' @export
listify <- function(l, ns=NULL) {
  if(is.null(ns))
    ns<-names(l)
  for(n in ns) {
    if(length(l[[n]])>1)
      l[[n]] <- list(l[[n]])
#    else
#      l[[n]] <- NULL
  }
  l
}

#'
#'
#' @export

run_all.gamma <- function(bt=config(path)$gridPath, enabled=NULL, run_name = run_name_today(), parallel=T) {
  if(is.character(bt))
    bt <- yaml::yaml.load_file(bt)

  `%fun%` <- `%do%`
  if (parallel == TRUE){
    require(doParallel)
    cl <- makePSOCKcluster(detectCores())
    registerDoParallel(cl)
    `%fun%` <- `%dopar%`
  }
  
  mkdirs(paste0(bt$config$outdir, run_name))
  as.yaml(bt) %>% write(file=paste0(bt$config$outdir,run_name,"/", "grid.yaml"))
  
  bt$config <- list(no_commission=F, 
                    no_cache = T, # всегда из базы
                    no_save = F, # не писать кэш на диск
                    log_level = LOG$INFO,
                    log_stdout = LOG$WARN, 
                    zero_position_on_rolls = F,
                    zero_position_freq = NULL,
                    custom_roll = NULL,
                    perfs_freq = as.numeric(days(1)),
                    perfs_tz = as.integer(15),
                    roll_same_day_all_legs=T,
                    start=as_datetime("2011-01-01"), 
                    stop=as_datetime("2099-01-01")) %>% modifyList(bt$config) %>% parse_periods(c("zero_position_freq", "perfs_freq"))  %>% parse_dates(c("start","stop"))
  
  enabled <- ifnull(enabled, bt$config$enabled)
  status_file = paste0(bt$config$outdir,run_name,"/", "errors.log")
  strats <- bt$strategies %>% keep(function(st)!isTRUE(st$name %in% bt$config$disabled) && (is.null(enabled) || isTRUE(st$name %in% enabled)))
  all_res_file <- paste0(bt$config$outdir, run_name, "/", "results.csv")
  
  all_runs <- foreach::foreach(st = iterators::iter(strats), .errorhandling = "stop",  .packages = "ggplot2") %fun%  {
  #  for(st in strats) { 
    #tryCatch({
       {  
        bt$config$log_path <- paste0(bt$config$outdir,run_name, "/", st$name,".log") %>% path.expand()
        # init logging, see rticks.log
        init_spd_log(bt$config)
         
        wlog("*******************  B A C K T E S T ***********", st$name, "now", as.character(Sys.time()), "disabled", isTRUE((st$name %in% bt$config$disabled)))
        st$legs = st$legs %>% map(function(l) modifyList(leg_defaults, listify(l)))

        params <- st$legs %>% map(~ as_data_frame(.)) %>% bind_rows()
     
        cfg <- backtest_config_default %>% modifyList(bt$config)
        
        if(!is.null(st$config))
          cfg <- cfg %>% modifyList(st$config)
        if(!cfg$roll_months_ahead)
          cfg$roll_months_ahead<-NA
        if(!cfg$roll_day_of_month)
          cfg$roll_day_of_month<-NA

      
        if(!is.na(cfg$roll_months_ahead) || !is.na(cfg$roll_day_of_month))
          cfg$custom_roll <- roll_day(day_of_month=cfg$roll_day_of_month, months_ahead = cfg$roll_months_ahead)
        ifnull(st$params$active_contract,0) %>% map(function(ac) {
          wlog("CFG:\n", as.yaml(cfg))
          stparams <- params_defaults %>% modifyList(st$params)
          stparams$active_contract<-ac
          stparams<-stparams %>% expand.grid(stringsAsFactors=F)
          wlog("STPARAMS:\n", df_chr(stparams))
          params_ac <- params %>% mutate(active_contract=active_contract+ac, min_active_contract=min_active_contract+ac)
          wlog("LEGS:\n", df_chr(params_ac))
                    
          runs <- params_ac %>% backtest(stparams=stparams, "gamma", start=bt$config$start, stop=bt$config$stop, config=cfg, parallel = parallel) 
          cur<-cfg$currency
          runs <- runs %>% map(function(r){
            uniq_pars <- names(st$params) %>% keep(~ length(st$params[[.]])>1)
            stpuniq <-  r$stparams[uniq_pars]
            parvals <- map2(stpuniq,names(stpuniq), ~ paste0(.y,"~",.x))
            #browser()
            stfname <- paste0(st$name,".", parvals %>% paste.list(sep="."))
            outfile <- paste0(bt$config$outdir, run_name,"/", stfname)
            #pp <- params
            #pp$roll_pattern <- NULL
            #pp %>% write.csv(paste0(outfile,".params.csv"), row.names=F)
            
            #runs[[st$name]] <- r
            bt_reports(r, no_commission=bt$config$no_commission, currency=cur, currency_power = cfg$currency_power)
            cur<-r$currency
            plt<-bt_plot(r,no_gaps=F, maxpoints = 1000) # PLOT IN USD
            ggsave(paste0(outfile,".png"), plot=plt)
            wlog("saved ", paste0(outfile,".png"))
            r$results <- r$stparams %>% cbind(tail(r$metrics,1))
            r$results$returns_file <- paste0(stfname,".returns.csv")
            r$results$metrics_file <- paste0(stfname,".metrics.csv")
            r$results$schedule_file <- paste0(st$name,".",ifnull(r$stparams$active_contract,0),".schedule.csv")
            r$results$name <- st$name
            r$metrics %>% write.csv(paste0(bt$config$outdir, run_name,"/", r$results$metrics_file), row.names=F)
            returns.xts <- r$metrics %>%
              select(datetime, rtn) %>%
              write.csv(file = paste0(bt$config$outdir, run_name,"/", r$results$returns_file), row.names = F)
            r$params %>% write.csv(paste0(outfile,".params.csv"), row.names=F)
            r$schedule %>% write.csv(paste0(bt$config$outdir,run_name,"/",r$results$schedule_file), row.names=F)
            r$name <- st$name
            r$results %>% write.csv(file=paste0(all_res_file,".tmp"), row.names=F, append = T)
            r
          })
          runs %>% map(~as.list(.)) 
        }) %>% reduce(c)
      }
    #}, error = browser())
    #function(e){ 
    #  wlog("ERROR!", e)
    #  write(as.character(e), file=status_file, append=T)
    #  browser()
      #runs[[st$name]] <- e
    #  e
  }
  #all_runs <- c(all_runs)[[1]]
  #
  #
  all_runs <- all_runs %>% reduce(c)
  all_results <- all_runs %>% map_df(~ .$results)
  wlog("ALL RESULTS", all_res_file, "rows=", nrow(all_results))
  all_results %>% write.csv(file=all_res_file, row.names=F)
  #all_runs %>% setNames(all_runs%>%map(~.$name))
  all_runs
}
