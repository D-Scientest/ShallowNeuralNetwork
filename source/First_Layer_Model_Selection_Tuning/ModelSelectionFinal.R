source('source/Source_First_Layer_Model_Selection_Tuning/HMM_State_Model_Selection.R')

stateList <- c("activated","inhabited"); bandList <- 1:6
freqList <- 1:8; bandLagList <- 2; queryNum <- 115 

# Return MSE for the candidate model and the corresponding tunning parameters
for(freq_ in freqList){
  for(state_ in 1:2){
    ModelComp_Location <- NULL
    for(bandIdx_ in bandList){
      # Model comparing
      ModelComp_Location_ <- HMM_State_Model_Selection(freqIdx_ = freq_, bandIdx_ = bandIdx_,
                                state = stateList[state_], bandLag = bandLagList, queryNum = queryNum)
      ModelComp_Location <- rbind(ModelComp_Location, ModelComp_Location_)
    }
    ModelComp_Location_path <- paste0("data/ModelDecision/Linear_NonLinear/ModelComp_Location_freq",freq_,"_lag", bandLagList, "_state",state_,".csv")
    write.table(ModelComp_Location, ModelComp_Location_path, col.names = FALSE, row.names = FALSE, sep = ',')
  }
}




