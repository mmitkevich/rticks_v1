# input chunk should be already cleaned
'
###################################################################################################################
chunks <- query_candles(c("PL.NYMEX.1","GC.COMEX.1"), start = dt(2017), active_contract = 3) %>% fetch_all()
chunk <- clean.chunk(chunks[[1]], 
                     schedules = NULL, 
                     cut_minutes = 0, 
                     negative_bidask = TRUE,
                     zero_prices = TRUE)
weights <- list(PL.NYMEX = 2, GC.COMEX = -1)
sythetic.chunk(chunk, weights)
###################################################################################################################
chunks <- query_candles(c("FOAT.EUREX.3","FGBL.EUREX.3"), start = dt(2017), active_contract = 3) %>% fetch_all()
chunk <- clean.chunk(chunks[[1]], 
                     schedules = NULL, 
                     cut_minutes = 0, 
                     negative_bidask = TRUE,
                     zero_prices = TRUE)
weights <- list(FOAT.EUREX.3 = 2, FGBL.EUREX.3 = -1)
synthetic.chunk(chunk, weights)
###################################################################################################################
'

#' synthesize chunk by formula
#' 
#' @export
synthetic.chunk <- function(chunk, weights, powers, currencies, mpi=NULL) {
  quotes <- list()
  instruments <- names(weights)
  
  for (i in 1:length(instruments)) {
    quotes[[i]] <- chunk %>% filter(virtual_id == instruments[i]) %>% select(datetime,bid,ask)
    p <- powers[instruments[i]] 
    #browser()
    if(p<0)
      quotes[[i]] <- quotes[[i]] %>% rename(bid=ask,ask=bid)
    quotes[[i]] <- quotes[[i]] %>% mutate(bid=bid^p, ask=ask^p)
  }
  names(quotes) <- instruments
  
  for (i in 1:length(instruments)) {
    cur <- currencies[instruments[i]]
    w<-weights[instruments[i]]
    if(!is.na(cur)) {
      currency  <- if(w>=0) {
        quotes[[cur]] %>% mutate(cur=bid) %>% select(datetime, cur)
      } else {
        quotes[[cur]] %>% mutate(cur=ask) %>% select(datetime, cur)
      }
      quotes[[i]] <- quotes[[i]] %>% inner_join(currency, by="datetime") %>% mutate(bid=bid*cur, ask=ask*cur)
    }
    quotes[[i]] <- quotes[[i]]%>%mutate(bid=bid*w, ask=ask*w)
    if(w<0)
      quotes[[i]] <- quotes[[i]] %>% rename(bid=ask,ask=bid)
  }

  # apply powers
  
  bids <- quotes %>% map2(names(quotes), ~ .x%>%select(datetime, bid)%>%setNames(c("datetime",.y))) %>% reduce(inner_join, by="datetime")
  dts <- bids$datetime
  bids <- bids %>% select(-datetime) %>% reduce(`+`)
  asks <- quotes %>% map2(names(quotes), ~ .x%>%select(datetime, ask)%>%setNames(c("datetime",.y))) %>% reduce(inner_join, by="datetime") %>% select(-datetime) %>% reduce(`+`)
  if(!is.null(mpi)) {
    bids <- trunc(bids/mpi)*mpi
    asks <- trunc((asks*(1-1e-6))/mpi)*mpi+mpi
  }
  cat("merged ", nrow(quotes[[1]]), nrow(quotes[[2]]), "into", nrow(bids),"\n")
  

  if(length(bids)==0)
    return(data_frame())
  
  spread.chunk <- data_frame(datetime = dts, 
                             bid = bids, 
                             ask = asks, 
                             high = bid, 
                             low = ask, 
                             virtual_id = paste.list(names(weights), sep = "-"),
                             exante_id = paste.list(sort(unique(chunk$exante_id)), sep="-"))
  return(spread.chunk)
}

