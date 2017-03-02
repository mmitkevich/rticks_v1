IDpath <- "~/Instrument_Data/"
files <- list.files(path = IDpath)

globalConfigPath <- "~/Exante_R_config.yaml"
config <- yaml.load_file(globalConfigPath)


con <- dbConnect(drv = config$db$dbDriver,
                 dbname = config$db$dbname,
                 host = config$db$host,
                 port = config$db$port,
                 user = config$db$user,
                 password = config$db$password)



start_req <- "INSERT INTO quant_data.instruments (instrument_id, mpi, multiplier, currency, comission_fixed, instrument_class, active_contract) VALUES ('"

for (i in 1:length(files)) {
  file_data <- yaml.load_file(paste0(IDpath, files[i]))
  for (j in 1:length(file_data$activeMonths)) {
    month <- file_data$activeMonths[j]
    if (j == 1) {
      month_array <- paste0("[" ,month)
    } else {
      month_array <- paste0(month_array, ",", month)
    }
  }
  active_M <- paste0(month_array, "]")
  index <- regexpr( ".yaml",files[i])[1]
  exante_id <- substr(files[i], 1, index-1)
  long_request <- paste0(start_req, exante_id, "','" , file_data$mpi, "','" ,file_data$multiplier, "','" , file_data$currency, "','" , file_data$comission, "','M','" ,active_M, "') on conflict(instrument_id) do update set mpi = '", file_data$mpi,"';")
  dbGetQuery(con,  long_request)
}

dbDisconnect(con)