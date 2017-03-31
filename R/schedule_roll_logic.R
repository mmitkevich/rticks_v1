# TODO params <- active contracts need to fix
# write.csv(q$schedule, "~/vgo/Data/sched.csv")

##########################################################
# sched <- read.csv("~/vgo/Data/sched.csv")
# act_contracts <- list(PL.NYMEX = c(4,3,2), GC.COMEX = c(2,1))
##########################################################
# schedule.roll.logic(sched = sched, act_contracts = act_contracts)


schedule.roll.logic <- function(sched, act_contracts) {
  for (i in 1:length(names(act_contracts))) {
    filt_inst_schedule <- sched %>% filter(instrument_id == names(act_contracts)[i])
    for (j in 1:length(unique(filt_inst_schedule$datetime))) {
      contract <- unlist(act_contracts[i])[if (j %% length(unlist(act_contracts[i])) == 0) {length(unlist(act_contracts[i]))} 
                                           else {j - (j %/% length(unlist(act_contracts[i]))) * length(unlist(act_contracts[i]))}]
      line <- filt_inst_schedule %>% filter(datetime == unique(filt_inst_schedule$datetime)[j]) %>% filter(active_contract == contract)
      if (j == 1) {
        DF <- line
      } else {
        DF <- rbind(DF, line)
      }
    }
    if (i == 1) {
      DF_fin <- DF
    } else {
      DF_fin <- rbind(DF_fin, DF)
    }
  }
  DF_fin %>% arrange(datetime)
}
