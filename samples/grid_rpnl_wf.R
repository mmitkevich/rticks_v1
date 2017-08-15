#' ---
#' title: Adaptive Take Profit
#' author: mim@exante.eu
#' date: 2017-08-01
#' output:
#'    pdf_document:
#'      toc: false
#' ---

#+ echo=FALSE
#+ message=FALSE
#+ warnings=FALSE

library(rticks)
library(ggplot2)
library(grid)
library(gridExtra)
library('knitr')

options(debug=T)
#.<-cfg.reload()

#if(!in_knitr()) {
#  knitr_to_pdf("samples/grid_rpnl_wf.R")
#  stop()
#}


vplot <- function(...) {
  plts <- list(...)
  plts <- plts %>% map(ggplotGrob)
  grid.newpage()
  plt<-do.call(rbind,c(plts,size="last"))
  plt %>% grid.draw()
  plt
}

#+ echo=T  
objective = "rpnl"
params = "spread"
run_name = "20170803/120350-for_walk_forward-RGEBZ"
IIS = c(1, 2,  3, 5, 7, 10)
#+ echo=F

IIS.plot = c() %>% intersect(IIS)

bt = config.gamma("samples/grid_bt.yaml") %>% bt_load_results(run_name = run_name)

stnames <- unique(bt$results$symbol)

start<-as_datetime("2015-01-01")
stop<-as_datetime("2017-12-01")

enabled <-  stnames # c("PL.NYMEX.1-GC.COMEX.1") # 

for(stname in stnames %>% intersect(enabled)) {
  indxs <- which(bt$results$symbol==stname)
  metrics <- bt$metrics[indxs]
  results <- bt$results[indxs,]
  indx.max <- which.max(results$rpnl)
  spread.max <- results[indx.max,]$spread
  metrics.max <- metrics[[indx.max]] %>% mutate(iis=0, spread=spread.max)
  all_metrics <- metrics.max
  #' Generate a series of plots from a loop AAA
  #+ fig.width=10, fig.height=10
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
      inner_join(metrics.max%>%transmute(datetime=datetime,rpnl.max=rpnl),by="datetime")  %>% mutate(rpnl=rpnl-rpnl.max)
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
  g1 <- ggplot(all_metrics%>%filter(iis!=0), aes(x=datetime, y=rpnl, colour=factor(iis)))+geom_step()+ggtitle(stname)+theme_bw()+scale_colour_discrete(drop=TRUE)
  g2 <- ggplot(all_metrics%>%filter(iis %in% IIS.plot | iis==best.iis | iis==0), aes(x=datetime, y=spread, colour=factor(iis)))+geom_step()+theme_bw()
  g3 <- ggplot(all_metrics%>%filter(iis==0), aes(x=datetime, y=price, colour=symbol))+geom_step()+theme_bw()

  plt <-vplot(g1,g2,g3)
  plt %>% print()
  basedir <- paste0(bt$config$outdir,run_name,"/",results$name[1])
  ggsave(paste0(basedir,"/img/",results$name[1],".forward.active_contract~",results$active_contract[1],".iis~",iis_days,".png"), plot=plt)
  all_metrics %>% write.csv(file=paste0(basedir,"/res/",results$name[1],".forward.active_contract~",results$active_contract[1],".iis~",iis_days,".csv"))
  #g4 <- qplot((all_metrics%>%filter(what==best.what))$spread, geom="histogram", title=paste0(stname," best spread"))
  #grid.arrange(g1,g2,g3) %>% print()
  #browser()
  #g4 %>% print()
}

