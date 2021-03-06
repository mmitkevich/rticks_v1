#' add metrics
#' 
#' @export
metrics.gamma <- function(env, no_commission=F, currency=NULL) {
  #params = attr(perfs, "params")
  qtys <- env$metrics
  if(has_name(qtys, "commission"))
    qtys <- qtys %>% select(-commission)
  pars <- env$params %>% select(symbol, multiplier, commission, spread) %>% mutate(commission=na_replace(commission,0))
  if(has_name(qtys, "spread"))
    pars$spread <- NULL
  qtys <- qtys %>% inner_join(pars, by="symbol")
  #browser()
  spreadi <- lag(qtys$spread)
  spreadi[1] <- spreadi[2]
  qtys <- qtys %>% mutate(spread1=spreadi,
    rpnl = (pmin(qty_buy, qty_sell)-lag(pmin(qty_buy, qty_sell),default=0)) * spread1 * multiplier
  ) %>% mutate(rpnl=cumsum(na_replace(rpnl)))
  
  if(!no_commission)
    qtys <- qtys %>% mutate(
      commission = commission*(qty_buy+qty_sell),
      rpnl = rpnl - commission, 
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
  qtys<- qtys %>% select(-multiplier)
  qtys
}


#'
#'
#' @export
leg_defaults <- list(symbol=NA, weight=1, power=1, active_contract=0, min_active_contract=0)

#'
#'
#' @export
params_defaults <- list(gamma.buy=1, gamma.sell=1, risk.buy=NA, limit.buy=NA, stop.buy=NA, limit.sell=+Inf, stop.sell=+Inf, spread=1, pos=NA, active_contract=0, kpos=0)

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
add_others <- function(r, s) {
  for(n in names(s)) {
    if(!(n %in% r))
      r[n] <- s[n]
  }
  r
}

#'
#'
#' @export
order_cols <- function(df) {
  df[,order(colnames(df))]
}

#'
#'
#' @export
run_all.gamma <- function(bt=config(path)$gridPath, 
                          enabled=NULL, 
                          run_name = run_name_today(), 
                          keep_data=F, 
                          IIS=NULL, 
                          OOS=NULL,
                          IIS.plot=c(), 
                          best_interval=1) {
  if(is.character(bt))
    bt <- yaml::yaml.load_file(bt)
  bt$run_name <- run_name
  mkdirs(paste0(bt$config$outdir, run_name))
  list(config=bt$config,strategies=bt$strategies) %>% as.yaml() %>%  write(file=paste0(bt$config$outdir,run_name,"/", "grid.yaml"))
  
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
                    start = as_datetime("2011-01-01"), 
                    stop = as_datetime("2099-01-01")) %>% modifyList(bt$config) %>% 
              parse_periods(c("zero_position_freq", "perfs_freq"))  %>% 
              parse_dates(c("start","stop"))
  
  init_spd_log(bt$config)
  wlog("LOG INITIALIZED")
  
  enabled <- ifnull(enabled, bt$config$enabled)
  IIS <- ifnull(IIS, bt$config$IIS)
  OOS <- ifnull(OOS, bt$config$OOS)
  
  status_file = paste0(bt$config$outdir,run_name,"/", "errors.log")
  strats <- bt$strategies %>% keep(function(st) 
    !isTRUE(st$name %in% bt$config$disabled) && (is.null(enabled) || isTRUE(st$name %in% enabled)))
  all_res_file <- paste0(bt$config$outdir, run_name, "/", "results.csv")
  
  qtile <- 0
  
  all_runs <- strats %>% parmap(function(st) { 
      ldir <- paste0(bt$config$outdir,run_name, "/",st$name,"/log")
      bt$config$log_path <- paste0(ldir, "/", st$name, ".log") %>% path.expand()
      mkdirs(ldir)
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
      acs <- ifnull(st$params$active_contract,0)
      rs <- acs %>% parmap(function(ac) {
        init_spd_log(bt$config)
        wlog("CFG:\n", as.yaml(cfg))
        stparams <- params_defaults %>% modifyList(st$params)
        stparams$active_contract<-ac
        stparams<-stparams %>% expand.grid(stringsAsFactors=F) %>% as_data_frame()
        #browser()
        stparams <- stparams %>% map(unlist) %>% as_data_frame()
        #stparams$spread <- unlist(stparams$spread)
        #stparams$kpos <- unlist(stparams$spread)
        
        wlog("STPARAMS:\n", df_chr(stparams))
        params_ac <- params %>% mutate(active_contract=ifelse(active_contract==-1, 1, active_contract+ac), 
                                       min_active_contract=ifelse(min_active_contract==-1, 1, min_active_contract+ac))
        wlog("LEGS:\n", df_chr(params_ac))
                  
        runs <- params_ac %>% backtest(stparams=stparams, "gamma", start=bt$config$start, stop=bt$config$stop, config=cfg) 
        data<-runs[[1]]$data
        cur<-cfg$currency
        outdir <- paste0(bt$config$outdir, run_name, "/",st$name) # "/", stfname
        runs <- runs %>% map(function(r){
          uniq_pars <- names(st$params) %>% keep(~ length(st$params[[.]])>1) %>% keep (~ .!="active_contract")
          stpuniq <-  r$stparams[uniq_pars]
          parvals <- map2(stpuniq,names(stpuniq), ~ paste0(.y,"~",.x))
          stfname <- paste0(st$name,".", ac, ".",parvals %>% paste.list(sep="."))
          mkdirs(outdir)
          mkdirs(paste0(outdir,"/img"))
          mkdirs(paste0(outdir,"/img/wf"))
          mkdirs(paste0(outdir,"/res"))
          #pp <- params
          #pp$roll_pattern <- NULL
          #pp %>% write.csv(paste0(outfile,".params.csv"), row.names=F)
          
          #runs[[st$name]] <- r
          if(!is.null(r$perfs)) {
            bt_reports(r, no_commission=bt$config$no_commission, currency=cur, currency_power = cfg$currency_power)
            cur <- r$currency
            plt <- bt_plot(r,no_gaps=F, maxpoints = 1000) # PLOT IN USD
            ggsave1(paste0(outdir,"/img/", stfname, ".png"), plot=plt)
            r$results <- tail(r$metrics, 1) %>% add_others(r$stparams) %>% order_cols()
            #r$results$returns_file <- paste0(stfname,".returns.csv")
            r$results$metrics_file <- paste0("res/", stfname, ".metrics.csv")
            cat("SAVING TO", r$results$metrics_file)
            r$results$schedule_file <- paste0("res/", st$name, ".", ifnull(r$stparams$active_contract, 0), ".schedule.csv")
            r$results$name <- st$name
            r$metrics %>% write.csv(paste0(outdir, "/", r$results$metrics_file), row.names=F)
            #returns.xts <- r$metrics %>%
            #  select(datetime, rtn) %>%
            #  write.csv(file = paste0(bt$config$outdir, run_name,"/", r$results$returns_file), row.names = F)
            #r$params %>% write.csv(paste0(outdir,"/results/",".params.csv"), row.names=F)
            r$schedule %>% write.csv(paste0(outdir,"/",r$results$schedule_file), row.names=F)
            r$name <- st$name
            tmpfname <- all_res_file# paste0(all_res_file,".tmp")
            wlog("results saved to ",tmpfname)
            r$results <- r$results %>% mutate(iis=NA) %>% order_cols()
            r$results %>% write.table(file=tmpfname, row.names=F, append = T, sep=",", col.names = T) # !file.exists(tmpfname)
            if(!keep_data) {
              r$data <- NULL
              r$data.spread <- NULL
            }
          }
          r
        })
        if(length(IIS)>0) {
          results <- runs %>% map_df(~ .$results)
          metrics <- runs %>% map(~ .$metrics)
          #browser()
          
          #r1 <- metrics %>% map_df(~ .[best_interval*nrow(.),])
          #results$rpnl <- r1$rpnl
          
          indx.max <- which.max(results$rpnl)
          spread.max <- results[indx.max,]$spread
          metrics.max <- metrics[[indx.max]] %>% mutate(iis=0, spread=spread.max)
          oos <- list()
          for(iis_indx in seq(1, length(IIS))) {
            iis_days <- IIS[iis_indx]
            oos_days <- ifelse(iis_indx<=length(OOS), OOS[iis_indx], 1)
            iis.lag <- trunc(iis_days/oos_days)
            wlog("WF", st$name,".",ac,"iis",iis_days,"oos",oos_days, "lag",iis.lag)
            R <- seq(1, length(metrics)) %>% map(function(i) {
              rs <- results[i,]
              
              mt <- metrics[[i]] %>% mutate(
                  dhl = (roll_max(price_high,trunc(3),fill=NA) - roll_min(price_low,trunc(3),fill=NA)),
                  drange = (dhl/2/rs$mpi+pos)*dhl
              )
              perfs_tz = bt$config$perfs_tz
              mtd <- mt %>% group_by(trunc((as.numeric(datetime)-60-perfs_tz*60*60)/(24*60*60*oos_days))) %>% 
                filter(row_number()==n()) %>% as_data_frame()
              #browser()
              mtd$spread <- rs$spread
              stopifnot(iis.lag>0)
              mt1 <- mtd %>% mutate(
                is_qty_buy = qty_buy - lag(qty_buy, n=iis.lag, default=0),
                is_qty_sell = qty_sell - lag(qty_sell, n=iis.lag, default=0),
                oos_qty_buy = lead(qty_buy) - qty_buy,
                oos_qty_sell = lead(qty_sell) - qty_sell,
                is_lrpnl = pmin(is_qty_buy, is_qty_sell)*spread,
                oos_lrpnl = pmin(oos_qty_buy, oos_qty_sell)*spread,
                is_rpnl = rpnl - lag(rpnl, n=iis.lag, default=0), 
                oos_rpnl = lead(rpnl) - rpnl,
                is  = is_lrpnl/drange,
                oos = oos_lrpnl/drange
              )
              #browser()
              #if(isTRUE(cfg$wf_analysis)){
                plt<-ggplot(mt1, aes(x=is, y=oos_lrpnl)) + geom_point() + ggtitle("lrpnl") + geom_smooth(method="lm")
                ggsave1(paste0(outdir,"/img/wf/", st$name, ".", ac,".FIT.lrpnl.",iis_days,"-",oos_days,".spread~",rs$spread,".png"), plot=plt)
                
                plt<-ggplot(mt1, aes(x=is, y=oos_rpnl)) + geom_point() + ggtitle("rpnl") + geom_smooth(method="lm")
                ggsave1(paste0(outdir,"/img/wf/", st$name, ".", ac,".FIT.rpnl.",iis_days,"-",oos_days,".spread~",rs$spread,".png"), plot=plt)
    
                plt<-ggplot(mt1, aes(x=is_lrpnl, y=is_rpnl)) + geom_point() + ggtitle("lrpnl->rpnl") + geom_smooth(method="lm")
                ggsave1(paste0(outdir,"/img/wf/", st$name, ".", ac,".lrpnl_rpnl.",iis_days,"-",oos_days,".spread~",rs$spread,".png"), plot=plt)
              #}
              #browser()
              mt1
            }) %>% bind_rows()
            R %>% write.csv(paste0(outdir, "/", paste0("res/",st$name,".",ac,".WF.", iis_days,"-",oos_days,".csv")), row.names=F)
            metrics.oos <- R %>% group_by(datetime) %>% arrange(-is) %>%
              filter(row_number()==1+trunc((n()-1)*qtile)) %>% # +
              arrange(datetime)  %>% as_data_frame() %>% arrange(datetime)
            metrics.oos.max <- R %>% group_by(datetime) %>% arrange(-oos) %>% filter(row_number()==1) %>% as_data_frame() %>% arrange(datetime) 
            
            metrics.oos <- metrics.oos %>% #mutate(rpnl=cumsum(oos_rpnl)) %>% 
              mutate(iis=iis_days) #%>% 
              #inner_join(metrics.max%>%transmute(datetime=datetime,rpnl.max=rpnl),by="datetime") %>% 
              #inner_join(metrics.oos.max %>% transmute(datetime=datetime,rpnl.oos.max=rpnl), by="datetime")
            
            #      if(plot_delta)
            #        metrics.oos <- metrics.oos %>% mutate(rpnl=rpnl-rpnl.max)
            #browser()  
            
            runbt <- function(spread_signal=data_frame(datetime=as_datetime(numeric()), value=numeric(), virtual_id=character()), type="OOS") {
              wfstparams<-stparams[indx.max,] %>% as_data_frame()
              siglist <- list()
              if(nrow(spread_signal)>0) {
                wfstparams$spread <- spread_signal$value[1]
                siglist <- list(spread=spread_signal %>% mutate(datetime=datetime+0.1))
              }
              #browser()
              r <- params_ac %>% backtest(
                stparams = wfstparams %>% mutate(iis=iis_days), 
                "gamma", 
                start = bt$config$start, 
                stop = bt$config$stop, 
                config = cfg, 
                signals = siglist,
                data = data) 
              r <- r[[1]]
              bt_reports(r, 
                         signals = 
                           spread_signal %>% rename(spread=value) # %>% mutate(datetime = datetime+bt$config$perfs_freq)
                         , 
                         no_commission = bt$config$no_commission, 
                         currency = cfg$currency, 
                         currency_power = cfg$currency_power)
              #browser()
              r$name <- paste0(st$name,".",ac,".",type,".", iis_days,"-",oos_days)
              r$metrics$symbol <- r$name
              r$schedule_file <- NA
              r$metrics_file <- NA
              r$results <- tail(r$metrics,1) %>% add_others(r$stparams)
              if(!keep_data) {
                r$data <- NULL
                r$data.spread <- NULL
              }
              if(nrow(r$metrics)>0) {
                r$results$metrics_file <- paste0("res/", r$name, ".metrics.csv")
                r$results$schedule_file <- paste0("res/", r$name, ".", ifnull(r$results$active_contract, 0), ".schedule.csv")
                r$results$name <- r$name
                r$results$iis <- iis_days
                r$results$oos <- oos_days
                r$metrics %>% write.csv(paste0(outdir, "/", r$results$metrics_file), row.names=F)
              }
              r$results <- r$results %>% order_cols()
              #plt<-vplot(plt, ggplot(metrics.oos, aes(x=datetime,y=spread))+geom_line()+theme_bw())
              r            
            }
  
            spread1 <- runs[[1]]$params$spread
            spread.max <- runs[[indx.max]]$params$spread
            
            signal <- metrics.oos %>% select(datetime, spread) %>% rename(value=spread) %>% mutate(virtual_id=results$symbol[1])
            signal.max <- metrics.oos.max %>% select(datetime, spread) %>% rename(value=spread) %>% mutate(virtual_id=results$symbol[1])
  
            r <- runbt(signal, "OOS")
            r.max <- runbt(signal.max,"MAX")
            #browser()
            
            
            combined_metrics <-  r$metrics  %>% bind_rows(r.max$metrics) %>% 
              bind_rows(runs[[1]]$metrics %>% mutate(symbol=paste0(symbol,".spread~",spread1))) %>% 
              bind_rows(runs[[indx.max]]$metrics %>% mutate(symbol=paste0(symbol,".spread~",spread.max)))
            
            #sym.max <- r.max$metrics$symbol %>% head(1)
            #combined_metrics <- combined_metrics %>% arrange(datetime) %>% left_join(
            #  combined_metrics %>% filter(symbol==sym.max) %>% select(datetime,rpnl) %>% rename(rpnl0=rpnl), by="datetime") %>% 
            #mutate(xpnl=rpnl-rpnl0)
            
            plt <- plot_bt(combined_metrics, enabled = c("pnl","pos","price","rpnl","spread"),maxrows = 800) # PLOT IN USD
            ggsave1(paste0(outdir,"/img/", r$name, ".png"), plot=plt)
            
            wlog("results saved to ",all_res_file)
            r$results %>% write.table(file=all_res_file, row.names=F, append = T, sep=",", col.names = T) #!file.exists(all_res_file)
            plt.histo <- ggplot(r$metrics, aes(spread)) + geom_histogram()
            ggsave1(paste0(outdir,"/img/", paste0(r$name,".spread"), ".png"))
  
            runs <- c(runs,r)
            oos <- c(oos, r$metrics)
            gc()
          }
        }
        #browser()
        #browser()
        flush_spd_log()
        runs #%>% map(~as.list(.)) 
      })
      rs %>% reduce(c)
  })
  all_runs <- all_runs %>% reduce(c)
  bt$results <- all_runs %>% map_df(~ .$results)
  wlog("ALL RESULTS", all_res_file, "rows=", nrow(bt$results))
  bt$results  %>% write.csv(file=all_res_file, row.names=F)
  #all_runs %>% setNames(all_runs%>%map(~.$name))
  bt$runs <- all_runs
  bt$metrics <- all_runs %>% map(~ .$metrics)
  bt
}

#' 
#' @export
walk_forward.gamma <- function(bt=config(gamma)$gridPath, run_name, IIS, IIS.plot=c(), best_interval=1) {
  if(is.character(bt))
    bt <- config.gamma(bt)
  if(is.null(bt$runs))
    bt <- bt %>% bt_load_results(run_name = run_name)
  stnames <- unique(bt$results$symbol)
  start<-as_datetime("2015-01-01")
  stop<-as_datetime("2017-12-01")
  
  bt$forward <- list()
  for(stname in stnames) {
    indxs <- which(bt$results$symbol==stname)
    metrics <- bt$metrics[indxs]
    results <- bt$results[indxs,]
    r1 <- metrics %>% map_df(~ .[best_interval*nrow(.),])
    results$rpnl <- r1$rpnl
    indx.max <- which.max(results$rpnl)
    spread.max <- results[indx.max,]$spread
    metrics.max <- metrics[[indx.max]] %>% mutate(iis=0, spread=spread.max)
    all_metrics <- metrics.max
    for(iis_days in IIS) {
      R <- seq(1, length(metrics)) %>% map(function(i) {
        mt <- metrics[[i]]
        rs <- results[i,]
        mt1 <- mt %>% mutate(is = rpnl - lag(rpnl, iis_days), 
                             oos = lead(rpnl) - rpnl)
        mt1$spread <- rs$spread
        mt1
      }) %>% bind_rows()
      metrics.oos <- R %>% group_by(datetime) %>% arrange(-is) %>% filter(row_number()==1) %>% 
        arrange(datetime)  %>% as_data_frame() %>% arrange(datetime)
      metrics.oos <- metrics.oos %>% mutate(rpnl=cumsum(oos)) %>% mutate(iis=iis_days) %>% 
        inner_join(metrics.max%>%transmute(datetime=datetime,rpnl.max=rpnl),by="datetime")  
#      if(plot_delta)
#        metrics.oos <- metrics.oos %>% mutate(rpnl=rpnl-rpnl.max)
      #browser()  
      all_metrics <- bind_rows(all_metrics, metrics.oos)
      
      #print(ggplot(bind_rows(rpnl.oos,rpnl.max), aes(x=datetime, y=rpnl, colour=symbol))+geom_line()+ggtitle(paste(symbol, "iis",iis_days)))
      
      #    rpnl  <- rpnl.oos$rpnl[nrow(rpnl.oos)-1]
      #    rpnls <- rpnls %>% bind_rows(data_frame(iis_days=iis_days, rpnl=rpnl))
    }
    all_metrics<-all_metrics %>% filter(datetime<max(all_metrics$datetime)-days(2))
    
    dt.max<-max(all_metrics$datetime)
    all_metrics$iis <- as.integer(all_metrics$iis)  
    all_metrics$oos <- as.integer(all_metrics$oos)  
    best.iis <- ((all_metrics %>% filter(datetime==dt.max) %>% filter(iis!=0) %>% arrange(-rpnl) %>% select(rpnl, iis))%>%head(1))$iis
    g1 <- ggplot(all_metrics, aes(x=datetime, y=rpnl, colour=factor(iis)))+geom_step()+ggtitle(stname)+theme_bw()+scale_colour_discrete(drop=TRUE)
    g2 <- ggplot(all_metrics, aes(x=datetime, y=rpnl-rpnl.max, colour=factor(iis)))+geom_step()+ggtitle(stname)+theme_bw()+scale_colour_discrete(drop=TRUE)
    g3 <- ggplot(all_metrics%>%filter(iis %in% IIS.plot | iis==best.iis | iis==0), aes(x=datetime, y=spread, colour=factor(iis)))+geom_step()+theme_bw()
    g4 <- ggplot(all_metrics%>%filter(iis==0), aes(x=datetime, y=price, colour=symbol))+geom_step()+theme_bw()
    g5 <- ggplot(all_metrics%>%filter(iis !=0), aes(x=datetime, y=spread, colour=factor(iis)))+geom_step()+theme_bw()+facet_grid(iis ~ ., scales = "free_y")
    plt <-vplot(g1,g2,g3,g4)
    #plt %>% print()
    basedir <- paste0(bt$config$outdir,run_name,"/",results$name[1])
    pngfile<-paste0(basedir,"/img/",results$name[1],".forward.active_contract~",results$active_contract[1],".png")
    wlog("saving ",pngfile)
    ggsave(pngfile, plot=plt)
    pngfile_spread<-paste0(basedir,"/img/",results$name[1],".params~spread.active_contract~",results$active_contract[1],".png")
    ggsave(pngfile_spread, plot=g5)
    all_metrics %>% write.csv(file=paste0(basedir,"/res/",results$name[1],".forward.active_contract~",results$active_contract[1],".csv"))
    #g4 <- qplot((all_metrics%>%filter(what==best.what))$spread, geom="histogram", title=paste0(stname," best spread"))
    bt$forward <- c(bt$forward, list(all_metrics))
  }
  names(bt$forward) <- stnames
  bt
}

#' @export
config.gamma <- function(file = config(path)$gridPath) {
  yaml.load_file(file)
}

#' @export
bt_list_runs <-function(bt = config.gamma()) {
  #list.dirs(bt$config$outdir,full.names = F) %>% keep(~ nchar(.)>0)
  src<-bt$config$outdir
  list.dirs(src,full.names=F,recursive=F) %>% map(~ paste0(.,"/",list.dirs(paste0(src,.), full.names=F, recursive=F))) %>% reduce(c)
}

#' @export
bt_load_results <- function(bt, run_name = NULL) {
  if(is.null(run_name))
    run_name <- bt_list_runs(bt) %>% tail(1)
  path <- paste0(bt$config$outdir, run_name, "/results.csv")
  bt$path <- path
  bt$results <- read.csv(path, stringsAsFactors = F) %>% as_data_frame()
  bt$results$datetime <- as_datetime(bt$results$datetime)
  bt$metrics <- (bt$results %>% by_row(function(rs) {
    mt <- read.csv(paste0(bt$config$outdir, run_name,"/",rs$name,"/",rs$metrics_file), stringsAsFactors = F) %>% as_data_frame()
    mt$datetime <- as_datetime(mt$datetime)
    mt
  }))$.out
  bt
}


#' @export
str_left <- function(s, n=0) {
  s %>% map(~ substr(., start=0,stop=nchar(.)-n))
}

#' @export
as.equal.weights <- function(weights) {
  weight.equal <- weights
  for (i in 1:nrow(weight.equal)){
    weight.equal[i,] <- apply(weight.equal[i,], 2, function(x) ifelse(x > 0, 1/sum(weight.equal[i,]!=0), 0))
  }
}

#'
#' @export
all_metrics <- function(bt) {
  seq(1, length(bt$results)) %>% map(function(i){
    m <- bt$metrics[i]
    m$spread <- bt$results$spread[i]
    m
  }) %>% reduce(bind_rows)
}