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
    rtn = (pnl-lag(pnl))/lag(drisk))
  qtys<- qtys %>% select(-spread,-multiplier) %>% gather(metric, value, -datetime, -symbol)
  qtys
}


#'
#'
#' @export
leg_defaults <- list(symbol=NA, weight=0, power=1, active_contract=1, min_active_contract=1)

#'
#'
#' @export
params_defaults <- list(gamma.buy=1, gamma.sell=1, risk.buy=NA, limit.buy=NA, stop.buy=NA, limit.sell=+Inf, stop.sell=+Inf, spread=1, pos=NA)

#'
#'
#' @export
run_name_today <- function() strftime(Sys.Date(),"%Y%m%d")

#'
#'
#' @export
run_name_unique <- function() paste0(strftime(Sys.time(), "%Y%m%d_%H%M%S"))

#'
#'
#' @export
run_all.gamma <- function(bt=config(path)$gridPath, enabled=NULL, run_name = run_name_today()) {
  if(is.character(bt))
    bt <- yaml::yaml.load_file(bt)
  
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
                    roll_same_day_all_legs=F,
                    start=as_datetime("2011-01-01"), 
                    stop=as_datetime("2099-01-01")) %>% modifyList(bt$config) %>% parse_periods(c("zero_position_freq", "perfs_freq"))  %>% parse_dates(c("start","stop"))
  # init logging, see rticks.log
  init_spd_log(bt$config)
  
  runs <- list()
  
  enabled <- ifnull(enabled, bt$config$enabled)
  status_file = paste0(bt$config$outdir,run_name,"/", "errors.log")
  for(st in bt$strategies) { 
    tryCatch({
      if(!isTRUE(st$name %in% bt$config$disabled) && (is.null(enabled) || isTRUE(st$name %in% enabled))) {  
        wlog("*******************  B A C K T E S T ***********", st$name, "now", as.character(Sys.time()), "disabled", isTRUE((st$name %in% bt$config$disabled)))
        st$legs = st$legs %>% map(function(l) {  
          if(length(l$roll_pattern)>0)
            l$roll_pattern <- list(l$roll_pattern)
          else
            l$roll_pattern <- NULL
          leg_defaults %>% modifyList(l)
        })
        
        params <- st$legs %>% map(~ as_data_frame(.)) %>% bind_rows()
        stparams <- params_defaults %>% modifyList(st$params)
        for(p in names(stparams)) {
          params[[p]] <- stparams[[p]]
        }
        wlog("PARAMS:")
        print(params)
        
        outfile <- paste0(bt$config$outdir, run_name,"/", st$name)
        pp <- params
        pp$roll_pattern <- NULL
        pp %>% write.csv(paste0(outfile,".params.csv"), row.names=F)
        
        cfg <- backtest_config_default %>% modifyList(bt$config)
        if(!is.null(st$config))
          cfg <- cfg %>% modifyList(st$config)
        r <- params %>% backtest("gamma", start=bt$config$start, stop=bt$config$stop, config=cfg) 
        runs[[st$name]] <- r
        bt_reports(r, no_commission=bt$config$no_commission, currency=cfg$currency, currency_power = cfg$currency_power)
        plt<-bt_plot(r,no_gaps=F, maxpoints = 1000) # PLOT IN USD
        ggsave(paste0(outfile,".png"), plot=plt)
        
        r$metrics %>% write.csv(paste0(outfile,".metrics.csv"), row.names=F)
        returns.xts <- r$metrics %>%
          select(datetime, rtn) %>%
          write.csv(file = paste0(outfile,".returns.csv"), row.names = F)
        r$params %>% write.csv(paste0(outfile,".params.csv"), row.names=F)
        wlog("*********** E Q U I T Y ************* ", outfile)
        write(paste0("written results to", outfile), file=status_file)
      }
    }, error = function(e){ 
      wlog("ERROR!", e)
      write(as.character(e), file=status_file, append=T)
      runs[[st$name]] <- e
    })
  }  
  runs
}