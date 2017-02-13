#' returns fixed schedule filter
#' 
#' @export
fixed_session_schedule <- function(start="10:00", stop="16:00") {
  start <- strsplit(start, ":") %>% as.numeric()
  if(length(start)<2)
    start <- c(start, 0)
  stop <- strsplit(stop, ":") %>% as.numeric()
  if(length(stop))
    function(datetime) {
      
    }
}


#' query_schedule
#' 
#' @examples
#'  query_schedule("NYMEX")
#' @export
query_schedule <- function(instruments = NULL, ...) {
  query_quant_data(instruments, "quant_data.schedule", nm = "instrument_id", ...)
}
