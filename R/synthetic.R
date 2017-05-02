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
synthetic.chunk <- function(chunk, weights) {
  quotes <- list()
  instruments <- names(weights)
  
  for (i in 1:length(instruments)) {
    quotes[[i]] <- chunk %>% filter(virtual_id == instruments[i]) %>% select(datetime,bid,ask)
  }
  names(quotes) <- instruments
  
  merged_DF <- merge(quotes[[1]], quotes[[2]], by = "datetime")
  cat("merged ", nrow(quotes[[1]]), nrow(quotes[[2]]), "into", nrow(merged_DF),"\n")

  if(nrow(merged_DF)==0 && (nrow(quotes[[1]])>0 || nrow(quotes[[2]]>0))) {
    stop(paste("failed to merge ", instruments))
  }
  
  spread_bid <- weights[[2]] * (if(weights[[2]] >= 0) {merged_DF$bid.y} else {merged_DF$ask.y}) + weights[[1]] * (if(weights[[1]] >= 0) {merged_DF$bid.x} else {merged_DF$ask.x})
  spread_ask <- weights[[2]] * (if(weights[[2]] >= 0) {merged_DF$ask.y} else {merged_DF$bid.y}) + weights[[1]] * (if(weights[[1]] >= 0) {merged_DF$ask.x} else {merged_DF$bid.x})
  
  spread.chunk <- data_frame(datetime = merged_DF$datetime, 
                             bid = spread_bid, 
                             ask = spread_ask, 
                             high = bid, 
                             low = ask, 
                             virtual_id = paste.list(names(weights), sep = "-"),
                             exante_id = virtual_id)
  return(spread.chunk)
}

