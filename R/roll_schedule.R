#' get futures contract number
#' @examples 
#'  TODO:
#' @export
get_futures_contract_number <- function(exanteID) {
  contracSpecMonth <- data.frame(Number = c( 1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12),
                                 Letter = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                                 stringsAsFactors = FALSE)
  number <- which(substr(exanteID, nchar(exanteID)-4, nchar(exanteID)-4) == contracSpecMonth$Letter)
  if (length(number) == 0) {
    number <- 0
  }
  return(number)
}

#' get futures contract letter from exante_id
#' @examples 
#'   get_futures_contract_letter("VIX.CBOE.H2016")
#'  # H
#' @export
get_futures_contract_letter <- function(exanteID) {
  letter <- substr(exanteID, nchar(exanteID)-4, nchar(exanteID)-4)
  return(letter)
}

#' 
#' @export
.get_act_num_contract_after1 <- function(firstExanteID,  # first active contract
                                    activeMonthPattern,
                                    max_active_contract) {
  
  contracSpecMonth <- data.frame(Number = c( 1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12),
                                 Letter = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                                 stringsAsFactors = FALSE)
  
  exanteIDpatt <- substr(firstExanteID, 1, nchar(firstExanteID)-6)
  yearStart <- substr(firstExanteID, nchar(firstExanteID)-3, nchar(firstExanteID))
  
  exanteID <- character(0)
  activeContractNumber <- numeric(0)
  
  firstContrLetter <- substr(firstExanteID, nchar(firstExanteID)-4, nchar(firstExanteID)-4)
  patternFirstIndex <- which(which(firstContrLetter == contracSpecMonth$Letter) == activeMonthPattern)
  
  if (length(patternFirstIndex) == 0) {
    stop(paste("first active contract (",firstExanteID,") does not match the activeMonth pattern (",activeMonthPattern,")"))
  } else {
    for (i in 1:max_active_contract) {
      patternIndex <- (i+patternFirstIndex-2)%%length(activeMonthPattern) + 1
      patternNumber <- activeMonthPattern[patternIndex]
      contractLetter <- contracSpecMonth$Letter[patternNumber]
      year <- as.numeric(yearStart) + (i+patternFirstIndex-2)%/%length(activeMonthPattern)
      
      exanteID[i] <- paste0(exanteIDpatt, ".", contractLetter, year)
      activeContractNumber[i] <- i
    }
  }
  acttiveContractsDF <- as.data.frame(cbind(exanteID, activeContractNumber), stringsAsFactors = FALSE)
  return(acttiveContractsDF)
}

#' build active_contract updates schedule
#' 
#'   symbols %>% roll_schedule(start = as_dateime("2016-01-01"), stop=as_datetime("2017-01-01"))
#'   
#' @param start     дата начала работы робота по этому расписанию
#' @param stop      дата конца работы робота по этому расписанию
#' @param active_contract    паттерн экспирации (см. пример)
#' @param max_active_contract    максимальный номер контракта, который требуется включить в расписание
#' @return расписание экспираций = data.frame с колонками datetime, contract, exante_id, contract_sequence
#' 

#' @examples
#'   # выбрать контракты с first_notice_date в 2016 году из БД таблицы symbols
#'   symbols <- query_symbols(start=as_dt(2016), stop=as_dt(2017))
#'   
#'   # cформировать паттерн экспирации
#'   active_contract <- list(            # в примерах ниже считаем, что контракты:
#'      GOLD.FORTS = c(3, 6, 9,  12), #  истекают по 15м числам
#'      PL.NYMEX   = c(   6,     12)  #  истекаюи по 17м числам
#'      
#'   # посчитать расписание
#'   roll.sched <- roll_schedule(symbols, active_contract, max_active_contract=4, start=as_datetime("2016-01-01"), stop=as_datetime("2017-01-01"))
#'
#'   # напечатать расписание
#'   print(sched)
#
#'   # пример вывода для расписания
#'   
#'   # datetime       contract         exante_id           contract_sequence            # комментарий
#'   # 2015-12-15     GOLD.FORTS       GOLD.FORTS.H2016       1                         # мы начинаем 2016-01-01 с того что H2016 является 1м контрактом начиная с 2015-12-15
#'   # 2015-12-15     GOLD.FORTS       GOLD.FORTS.M2016       2                         # мы начинаем 2016-01-01 с того что M2016 является 2м контрактом
#'   # 2015-12-15     GOLD.FORTS       GOLD.FORTS.U2016       3                         # мы начинаем 2016-01-01 с того что U2016 является 3м контрактом
#'   # 2015-12-15     GOLD.FORTS       GOLD.FORTS.Z2016       4                         # мы начинаем 2016-01-01 с того что Z2016 является 4м контрактом

#'   # 2016-03-15     GOLD.FORTS       GOLD.FORTS.H2016       0                         # 2016-03-15 произойдет экспирация GOLD.FORTS.H2016 он станет 1->0
#'   # 2016-03-15     GOLD.FORTS       GOLD.FORTS.M2016       1                         # 2016-03-15 произойдет экспирация GOLD.FORTS.M2016 он станет 2->1
#'   # 2016-03-15     GOLD.FORTS       GOLD.FORTS.U2016       2                         # 2016-03-15 произойдет экспирация GOLD.FORTS.U2016 он станет 3->2
#'   # 2016-03-15     GOLD.FORTS       GOLD.FORTS.Z2016       3                         # 2016-03-15 произойдет экспирация GOLD.FORTS.Z2016 он станет 4->3

#'   # 2016-06-15     GOLD.FORTS       GOLD.FORTS.M2016       2                         # 2016-03-15 произойдет экспирация GOLD.FORTS.M2016 он станет 2->1
#'   # 2016-06-15     GOLD.FORTS       GOLD.FORTS.U2016       3                         # 2016-03-15 произойдет экспирация GOLD.FORTS.U2016 он станет 3->2
#'   # 2016-06-15     GOLD.FORTS       GOLD.FORTS.Z2016       4                         # 2016-03-15 произойдет экспирация GOLD.FORTS.Z2016 он станет 4->3
#'   # 2016-06-15     GOLD.FORTS       GOLD.FORTS.Р2017       4                         # 2016-03-15 произойдет экспирация GOLD.FORTS.Z2016 он станет 4->3

#'   
#'   # 2015-06-17     PL.NYMEX         PL.NYMEX.Z2016         1                         # мы начинаем 2016-01-01 с того что PL.NYMEX.Z2016 является 1м контрактом начиная с 2015-06-17 
#'   # 2016-12-17     PL.NYMEX         PL.NYMEX.Z2016         0                         # 2016-12-17 PL.NYMEX.Z2016 стал 0м контрактом
#'   
#' @export
roll_schedule <- function(.x,  
                          active_contract = NULL,
                          max_active_contract = 12, 
                          start = NULL, 
                          stop = NULL,
                          key = "instrument_id") {
  .x <- .x %>% parse_symbols(key=key)
  symbols <- .x[[key]]
  
  active_contract <- .x %>% listby(key) %>% 
                          map(~ .x$active_contract[[1]]) %>% 
                          modifyList(as.list(active_contract))
  #print(active_contract)
  
  contracSpecMonth <- data.frame(Number = c( 1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12),
                                 Letter = c("F","G","H","J","K","M","N","Q","U","V","X","Z"),
                                 stringsAsFactors = FALSE)

  # TODO: why query from the beginning of the time? start is enough
  data <- query_symbols(symbols, start = NULL) 
  start <- nget(min(data$fut_notice_first, na.rm=T), as_dt(1900))
  
  for (i in 1:length(symbols)) {
    # query_symbols(symbols[i], start = NULL) # MM: moved query out of cycle
    exanteIDDF <- data %>% filter(instrument_id==symbols[i]) 
    activeMonthNumericPattern <- active_contract[[i]]
    activeMonthLetterPattern <- contracSpecMonth$Letter[activeMonthNumericPattern]
 
    comDF <- exanteIDDF
    if(!is.null(start))
        comDF <-  comDF %>% filter(fut_notice_first >= start)
    if(!is.null(stop))
        comDF <- comDF %>% filter(fut_notice_first <= stop)
    
    comDF <- comDF %>% 
        #filter(get_futures_contract_letter(exante_id) %in%  activeMonthLetterPattern) %>% 
         filter(month %in% activeMonthNumericPattern) %>%
         arrange(fut_notice_first)
    
    for (z in 1:nrow(comDF)) {
      
      if (z == 1) {
        
        datetime <- rep(as.character(comDF$fut_notice_first[z]), max_active_contract)
        actContrNumberDF1 <- .get_act_num_contract_after1(comDF$exante_id[z], activeMonthNumericPattern, max_active_contract)
        resSchedDF.0 <- cbind(rep(as.character(start), max_active_contract), actContrNumberDF1, rep(symbols[i], nrow(actContrNumberDF1)))
        colnames(resSchedDF.0) <- c("datetime", "exanteID", "activeContractNumber", "exanteIDpat")
        resSchedDF.1 <- cbind(datetime, actContrNumberDF1, rep(symbols[i], nrow(actContrNumberDF1)))
        colnames(resSchedDF.1) <- c("datetime", "exanteID", "activeContractNumber", "exanteIDpat")
        resSchedDF <- rbind(resSchedDF.0, resSchedDF.1)
        
      } else {
        
        if (comDF$exante_id[z] == actContrNumberDF1$exanteID[2]) {
          
          datetime <- rep(as.character(comDF$fut_notice_first[z]), max_active_contract + 1)
          actContrNumberDF1 <- .get_act_num_contract_after1(comDF$exante_id[z], activeMonthNumericPattern, max_active_contract)
          actContrNumberDF <- rbind(c(comDF$exante_id[z-1], 0), actContrNumberDF1)
          exanteIDpat <- rep(symbols[i], nrow(actContrNumberDF))
          schedDFIt <- cbind(datetime, actContrNumberDF, exanteIDpat)
          
        } else {
          stop(paste0("error with contract -> ", actContrNumberDF1$exanteID[2]))
        }
        
        resSchedDF <- rbind(resSchedDF, schedDFIt)
      }
      
    }
    
    if (i == 1) {
      finSchedDF <- resSchedDF
    } else {
      finSchedDF <- rbind(finSchedDF, resSchedDF)
    }
    
  }
  finSchedDF$datetime <- as.Date(finSchedDF$datetime, format = "%Y-%m-%d")
  resDF <- arrange(finSchedDF, datetime) %>% 
    rename(exante_id=exanteID, active_contract=activeContractNumber, instrument_id=exanteIDpat) # TODO: fix the above code to produce theese columns names
  
  return(resDF)
}
