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
run_all.gamma <- function(bt=config(path)$gridPath, enabled=NULL, run_name = run_name_today(), parallel=T, keep_data=F, IIS=NULL, IIS.plot=c(),best_interval=1) {
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
                    roll_same_day_all_legs=T,
                    start=as_datetime("2011-01-01"), 
                    stop=as_datetime("2099-01-01")) %>% modifyList(bt$config) %>% parse_periods(c("zero_position_freq", "perfs_freq"))  %>% parse_dates(c("start","stop"))
  
  enabled <- ifnull(enabled, bt$config$enabled)
  IIS <- ifnull(IIS, bt$config$IIS)
  
  status_file = paste0(bt$config$outdir,run_name,"/", "errors.log")
  strats <- bt$strategies %>% keep(function(st)!isTRUE(st$name %in% bt$config$disabled) && (is.null(enabled) || isTRUE(st$name %in% enabled)))
  all_res_file <- paste0(bt$config$outdir, run_name, "/", "results.csv")
  
  all_runs <- strats %>% map(function(st) { 
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
        wlog("CFG:\n", as.yaml(cfg))
        stparams <- params_defaults %>% modifyList(st$params)
        stparams$active_contract<-ac
        stparams<-stparams %>% expand.grid(stringsAsFactors=F)
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
          mkdirs(paste0(outdir,"/res"))
          #pp <- params
          #pp$roll_pattern <- NULL
          #pp %>% write.csv(paste0(outfile,".params.csv"), row.names=F)
          
          #runs[[st$name]] <- r
          bt_reports(r, no_commission=bt$config$no_commission, currency=cur, currency_power = cfg$currency_power)
          cur<-r$currency
          plt<-bt_plot(r,no_gaps=F, maxpoints = 1000) # PLOT IN USD
          ggsave(paste0(outdir,"/img/", stfname, ".png"), plot=plt)
          r$results <- r$stparams %>% cbind(tail(r$metrics,1))
          #r$results$returns_file <- paste0(stfname,".returns.csv")
          r$results$metrics_file <- paste0("res/", stfname, ".metrics.csv")
          cat("SAVING TO",r$results$metrics_file)
          r$results$schedule_file <- paste0("res/", st$name, ".", ifnull(r$stparams$active_contract, 0), ".schedule.csv")
          r$results$name <- st$name
          r$metrics %>% write.csv(paste0(outdir, "/", r$results$metrics_file), row.names=F)
          #returns.xts <- r$metrics %>%
          #  select(datetime, rtn) %>%
          #  write.csv(file = paste0(bt$config$outdir, run_name,"/", r$results$returns_file), row.names = F)
          #r$params %>% write.csv(paste0(outdir,"/results/",".params.csv"), row.names=F)
          r$schedule %>% write.csv(paste0(outdir,"/",r$results$schedule_file), row.names=F)
          r$name <- st$name
          r$results %>% write.csv(file=paste0(all_res_file,".tmp"), row.names=F, append = T)
          if(!keep_data) {
            r$data<-NULL
            r$data.spread<-NULL
          }
          r
        })
        results <- runs %>% map_df(~ .$results)
        metrics <- runs %>% map(~ .$metrics)
        #browser()
        r1 <- metrics %>% map_df(~ .[best_interval*nrow(.),])
        results$rpnl <- r1$rpnl
        indx.max <- which.max(results$rpnl)
        spread.max <- results[indx.max,]$spread
        metrics.max <- metrics[[indx.max]] %>% mutate(iis=0, spread=spread.max)
        oos <- list()
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
          signal <- metrics.oos %>% select(datetime, spread) %>% rename(value=spread) %>% mutate(virtual_id=results$symbol[1])
          wfstparams <- head(stparams,1)
          wfstparams$spread <- metrics.oos$spread[1]
          #browser()
          r <- params_ac %>% backtest(stparams=wfstparams, "gamma", start=bt$config$start, stop=bt$config$stop, config=cfg, signals=list(spread=signal), data=data) 
          r <- r[[1]]
          bt_reports(r, no_commission=bt$config$no_commission, currency=cfg$currency, currency_power = cfg$currency_power)
          r$metrics <- r$metrics %>% inner_join(metrics.oos%>%select(datetime,spread), by="datetime")
          const_spread <- runs[[1]]$params$spread
          combined_metrics<-bind_rows(r$metrics, runs[[1]]$metrics %>% mutate(symbol=paste(symbol,const_spread)))
          plt<-plot_bt(combined_metrics,enabled = c("pnl","pos","price","rpnl","spread")) # PLOT IN USD
          r$name <- paste0(st$name,".",ac,".OOS.", iis_days)
          r$results <- r$stparams %>% cbind(tail(r$metrics,1))
          r$results$name<-r$name
          r$results$IIS<-iis_days
          r$results$metrics_file <- paste0("res/", r$name, ".metrics.csv")
          #plt<-vplot(plt, ggplot(metrics.oos, aes(x=datetime,y=spread))+geom_line()+theme_bw())
          ggsave(paste0(outdir,"/img/", r$name, ".png"), plot=plt)
          r$metrics %>% write.csv(paste0(outdir, "/", r$results$metrics_file), row.names=F)
          runs <- c(runs,r)
          oos <- c(oos, r$metrics)
        }
        #browser()
        #browser()
        runs
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