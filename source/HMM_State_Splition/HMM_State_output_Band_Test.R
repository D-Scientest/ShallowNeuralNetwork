#########################################################
#
# HMM data splition: 
#           splitting testing neural signal data into two parts, 
#           activated and inhabitated
#   output: 1. activated and inhabited neural data based on frequency band
#           2. sentence breakpoint for both types of data
#########################################################

sentenceNumber <- 5

source("source/Source_HMM_State_Splition/HMM_State_Total_Band.R")
for(bandIdx in 1:6){
  hmmState <- HMM_State_Total_Band(input_ = test_x, bandIdx_ = bandIdx, breakPoint_ = test_breakPoint,
                              senNumber_ = sentenceNumber,seed_ = 1234)
  path1 <- paste0("data/HMM_State_Test/inhabited_X_",bandIdx,'.csv')
  path2 <- paste0("data/HMM_State_Test/activated_X_",bandIdx,'.csv')
  # path3 <- paste0("data/HMM_State_Test/inhabited_Y_",bandIdx,'.csv')
  # path4 <- paste0("data/HMM_State_Test/activated_Y_",bandIdx,'.csv')
  path5 <- paste0("data/HMM_State_Test/inhabitedPeriod_breakPoint_",bandIdx,'.csv')
  path6 <- paste0("data/HMM_State_Test/activatedPeriod_breakPoint_",bandIdx,'.csv')
  path7 <- paste0("data/HMM_State_Test/inhabitedIdx_",bandIdx,'.csv')
  path8 <- paste0("data/HMM_State_Test/activatedIdx_",bandIdx,'.csv')
  
  
  write.table(hmmState$inhabitedPeriod_X, path1, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_X, path2, col.names = FALSE, row.names = FALSE, sep = ',')
  
  # write.table(hmmState$inhabitedPeriod_Y, path3, col.names = FALSE, row.names = FALSE, sep = ',')
  # write.table(hmmState$activatedPeriod_Y, path4, col.names = FALSE, row.names = FALSE, sep = ',')
  
  write.table(hmmState$inhabitedPeriod_breakPoint, path5, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_breakPoint, path6, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$inhabitedIdx_X, path7, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedIdx_X, path8, col.names = FALSE, row.names = FALSE, sep = ',')
}
