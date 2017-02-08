#########################################################
#
# HMM data splition: 
#           splitting testing neural signal data into two parts, 
#           activated and inhabitated
#   output: 1. activated and inhabited neural data based on location 
#           2. sentence breakpoint for both types of data
#########################################################
test_x <- as.data.frame(fread("data/test_X_ecog_switched.csv"))
# train_y_compress <- as.data.frame(fread("data/train_Y_ecog_compressed.csv"))
test_breakPoint <- as.data.frame(fread("data/test_breakpoints.txt"))
sentenceNumber <- 5

source("source/Source_HMM_State_Splition/HMM_State_Total_Location.R")
for(locIdx in 1:70){
  hmmState <- HMM_State_Total_Location(input_ = test_x, LocIdx_ = locIdx, breakPoint_ = test_breakPoint,
                                       senNumber_ = sentenceNumber,seed_ = 1234)
  path1 <- paste0("data/HMM_State_Location_Test/inhabited_X_",locIdx,'_Location.csv')
  path2 <- paste0("data/HMM_State_Location_Test/activated_X_",locIdx,'_Location.csv')
  # path3 <- paste0("data/HMM_State_Location_Test/inhabited_Y_",locIdx,'_Location.csv')
  # path4 <- paste0("data/HMM_State_Location_Test/activated_Y_",locIdx,'_Location.csv')
  path5 <- paste0("data/HMM_State_Location_Test/inhabitedPeriod_breakPoint_",locIdx,'_Location.csv')
  path6 <- paste0("data/HMM_State_Location_Test/activatedPeriod_breakPoint_",locIdx,'_Location.csv')
  path7 <- paste0("data/HMM_State_Location_Test/inhabitedIdx_",locIdx,'.csv')
  path8 <- paste0("data/HMM_State_Location_Test/activatedIdx_",locIdx,'.csv')
  
  
  write.table(hmmState$inhabitedPeriod_X, path1, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_X, path2, col.names = FALSE, row.names = FALSE, sep = ',')
  
  # write.table(hmmState$inhabitedPeriod_Y, path3, col.names = FALSE, row.names = FALSE, sep = ',')
  # write.table(hmmState$activatedPeriod_Y, path4, col.names = FALSE, row.names = FALSE, sep = ',')
  
  write.table(hmmState$inhabitedPeriod_breakPoint, path5, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedPeriod_breakPoint, path6, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$inhabitedIdx_X, path7, col.names = FALSE, row.names = FALSE, sep = ',')
  write.table(hmmState$activatedIdx_X, path8, col.names = FALSE, row.names = FALSE, sep = ',')
}
