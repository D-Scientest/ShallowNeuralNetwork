########################################################
#
# Find the optimal model for each electronode location/output frequency
#
########################################################
stateList <- c("activated","inhabited"); locList <- 1:70
locLagList <- 0; queryNum <- 115 

LocOptimal <- NULL
freqList <- 2
Model_Name <- c("OLS", "PiecewiseSpline", "SmoothSpline")
freqModel_activated <- matrix(0,nrow = 70, ncol = 0);
freqModel_inactivated <- matrix(0,nrow = 70, ncol = 0);

for(freqList_ in freqList){
  stateModelComp_Location_path <- paste0("data/ModelDecision/Linear_NonLinear/stateModelComp_Location_freq",freqList_,"_lag", locLagList, ".csv")
  table <-read.table(stateModelComp_Location_path, sep = ',')
  activatedOptimalIdx <- apply(cbind(table$V3[1:70],table$V4[1:70],
                                     table$V6[1:70]), 1, which.min)
  freqModel_activated <- cbind(freqModel_activated, Model_Name[activatedOptimalIdx])
  
  inactivatedOptimalIdx <- apply(cbind(table$V3[70:140],table$V4[70:140],
                                     table$V6[70:140]), 1, which.min)
  freqModel_inactivated <- cbind(freqModel_inactivated, Model_Name[inactivatedOptimalIdx])
  par(mfrow = c(1,2))
  plot(table$V3[1:70], type = "l", col = 'red', ylim = c(15.2,17))
  lines(table$V4[1:70], col = "green")
  lines(table$V6[1:70])
  
  plot(table$V3[70:140], type = "l", col = 'red', ylim = c(15.2,17))
  lines(table$V4[70:140], col = "green")
  lines(table$V6[70:140])
}

freqModel_activated_path <- paste0("data/ModelDecision/Linear_NonLinear/activated_AllFreq_Location.csv")
write.table(freqModel_activated, freqModel_activated_path, col.names = FALSE, row.names = FALSE, sep = ',')

freqModel_inactivated_path <- paste0("data/ModelDecision/Linear_NonLinear/inhabited_AllFreq_Location.csv")
write.table(freqModel_inactivated, freqModel_inactivated_path, col.names = FALSE, row.names = FALSE, sep = ',')

