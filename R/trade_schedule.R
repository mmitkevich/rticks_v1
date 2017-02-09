

instrumentID <- "FOAT.EUREX"
instrumentID <- "VIX.CBOT"


query_schedule(instrumentID)

data <- chunks[[1]]

dateTimeTestVector <- data$bid$datetime[1:100]

con

dbDisconnect(con)


wday(dateTimeTestVector)
hour(dateTimeTestVector)
minute(dateTimeTestVector)