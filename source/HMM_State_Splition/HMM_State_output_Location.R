#########################################################
#
# HMM data splition: 
#           splitting training neural signal data into two parts, 
#           activated and inhabitated
#   output: 1. activated and inhabited neural data based on location 
#           2. sentence breakpoint for both types of data
#########################################################
sentenceNumber <- 10

source("source/Source_HMM_State_Splition/HMM_State_Total_Location.R")
for(locIdx in 1:70){
  hmmState <- HMM_State_Total_Location(input_ = train_x_switched, LocIdx_ = locIdx, breakPoint_ = train_breakPoint,
                              output_ = train_y_compress,senNumber_ = sentenceNumber,seed_ = 1234)
  path1 <- paste0("data/HMM_State_Location_Train/inhabited_X_",locIdx,'_Location.csv')
  path2 <- paste0("data/HMM_State_Location_Train/activated_X_",locIdx,'_Location.csv')
  path3 <- paste0("data/HMM_State_Location_Train/inhabited_Y_",locIdx,'_Location.csv')
  path4 <- paste0("data/HMM_State_Location_Train/activated_Y_",locIdx,'_Location.csv')
  path5 <- paste0("data/HMM_State_Location_Train/inhabitedPeriod_breakPoint_",locIdx,'_Location.csv')
  path6 <- paste0("data/HMM_State_Location_Train/activatedPeriod_breakPoint_",locIdx,'_Location.csv')
  path7 <- paste0("data/HMM_State_Location_Train/inhabitedIdx_",locIdx,'.csv')
  path8 <- paste0("data/HMM_State_Location_Train/activatedIdx_",locIdx,'.csv')
  
  
  write.table(hmmState$inhabitedPeriod_X, path1, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_X, path2, col.names = FALSE, row.names = FALSE, sep = ',')
  
  write.table(hmmState$inhabitedPeriod_Y, path3, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_Y, path4, col.names = FALSE, row.names = FALSE, sep = ',')
  
  write.table(hmmState$inhabitedPeriod_breakPoint, path5, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_breakPoint, path6, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$inhabitedIdx_X, path7, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedIdx_X, path8, col.names = FALSE, row.names = FALSE, sep = ',')
}
