setwd('/home/zhenshan/Dropbox/STAT640CompetitionPrivate')
#####################
# OLS & Ridge
#####################
par(mfrow= c(2,4))
stateList <- c("activated","inhabited"); bandList <- 1:6
freqList <- 1; bandLagList <- 2; queryNum <- 115 

Model_Name <- c("Ridge", "OLS")
freqList <- c(1:8)
freqModel_activated <- matrix(0,nrow = 6, ncol = 0);
freqModel_inactivated <- matrix(0,nrow = 6, ncol = 0);
for(freqList_ in freqList){
  table <- NULL
  for(state_ in 1:2){
    ModelComp_Location_path <- paste0("data/output/Band_OLS_Ridge/ModelComp_Location_freq",freqList_,"_lag", bandLagList, "_state",state_,".csv")
    table <- rbind(table, read.table(ModelComp_Location_path, sep = ','))
  }
  
  activatedOptimalIdx <- apply(cbind(table$V1[1:6],table$V2[1:6]), 1, which.min)
  freqModel_activated <- cbind(freqModel_activated, Model_Name[activatedOptimalIdx])
  
  inactivatedOptimalIdx <- apply(cbind(table$V1[7:12],table$V2[7:12]), 1, which.min)
  freqModel_inactivated <- cbind(freqModel_inactivated, Model_Name[inactivatedOptimalIdx])
  
  # par(mfrow = c(1,2))
  plot(table$V2[1:6], type = "l", col = 'red')
  lines(table$V1[1:6], col = "green")
  
  plot(table$V2[7:12], type = "l", col = 'red')
  lines(table$V1[7:12], col = "green")#;browser()
}

freqModel_activated_path <- paste0("data/ModelDecision/activated_AllFreq_Band.csv")
write.table(freqModel_activated, freqModel_activated_path, col.names = FALSE, row.names = FALSE, sep = ',')

freqModel_inactivated_path <- paste0("data/ModelDecision/inhabited_AllFreq_Band.csv")
write.table(freqModel_inactivated, freqModel_inactivated_path, col.names = FALSE, row.names = FALSE, sep = ',')

#########################
# Ridge & Kernel
#########################
par(mfrow= c(2,4))
stateList <- c("activated","inhabited"); bandList <- 1:6
freqList <- 1; bandLagList <- 2; queryNum <- 115 
#####################
# Freq 1
#####################
freqOptimal1 <- NULL
state_ <- 1
for(bandIdx_ in 1:5){
  stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
  table <- read.table(stateModelComp_path, sep = ',')
  freqOptimal1 <- rbind(freqOptimal1,table)
}

state_ <- 2
for(bandIdx_ in bandList){
  if(bandIdx_ == 6){
    stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
    table <- read.table(stateModelComp_path, sep = ',')
    freqOptimal1 <- rbind(freqOptimal1,table)
  }
}
plot(freqOptimal1[,2], type = 'l', col = 'red')
lines(freqOptimal1[, 1])
lines()


###################
# Freq 2
###################
stateList <- c("activated","inhabited"); bandList <- 1:6
freqList <- 2; bandLagList <- 2; queryNum <- 115 

freqOptimal2 <- NULL
state_ <- 1
bandIdx_ <- 1
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal2 <- rbind(freqOptimal2,table)

state_ <- 2
bandIdx_ <- 6
stateModelComp_path <- paste0("data/output/Band_OLS_Ridge/ModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal2 <- rbind(freqOptimal2,table)

plot(freqOptimal2[,2], type = 'l', col = 'red')
lines(freqOptimal2[, 1])

###################
# Freq 3
###################
freqList <- 3
freqOptimal3 <- NULL
state_ <- 1
bandIdx_ <- 1
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal3 <- rbind(freqOptimal3,table)

state_ <- 2
bandIdx_ <- 6
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal3 <- rbind(freqOptimal3,table)

plot(freqOptimal3[,2], type = 'l', col = 'red')
lines(freqOptimal3[, 1])


###################
# Freq 4
###################
freqList <- 4
freqOptimal4 <- NULL
state_ <- 1
bandIdx_ <- 1
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal4 <- rbind(freqOptimal4,table)

state_ <- 2
bandIdx_ <- 6
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal4 <- rbind(freqOptimal4,table)

plot(freqOptimal4[,2], type = 'l', col = 'red', ylim = c(13,17.5))
lines(freqOptimal4[, 1])
###################
# Freq 5
###################
freqList <- 5
freqOptimal5 <- NULL
state_ <- 1
bandIdx_ <- 1
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal5 <- rbind(freqOptimal5,table)

state_ <- 2
bandIdx_ <- 6
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal5 <- rbind(freqOptimal5,table)

plot(freqOptimal5[,2], type = 'l', col = 'red', ylim = c(13,20))
lines(freqOptimal5[, 1])
###################
# Freq 6
###################
freqList <- 6
freqOptimal6 <- NULL
state_ <- 1
bandIdx <- c(1,6)
for(bandIdx_ in bandIdx){
  stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
  table <- read.table(stateModelComp_path, sep = ',')
  freqOptimal6 <- rbind(freqOptimal6,table)
}


state_ <- 2
bandIdx_ <- 6
stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
table <- read.table(stateModelComp_path, sep = ',')
freqOptimal6 <- rbind(freqOptimal6,table)

plot(freqOptimal6[,2], type = 'l', col = 'red',ylim = c(13,20))
lines(freqOptimal6[, 1])

###################
# Freq 7
###################
freqList <- 7
freqOptimal7 <- NULL
state_ <- 2
bandIdx <- c(1,6)
for(bandIdx_ in bandIdx){
  stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
  table <- read.table(stateModelComp_path, sep = ',')
  freqOptimal7 <- rbind(freqOptimal7,table)
}


plot(freqOptimal7[,2], type = 'l', col = 'red', ylim = c(13, 20))
lines(freqOptimal7[, 1])
###################
# Freq 8
###################
freqList <- 8
freqOptimal8 <- NULL
state_ <- 2
bandIdx <- c(1,6)
for(bandIdx_ in bandIdx){
  stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
  table <- read.table(stateModelComp_path, sep = ',')
  freqOptimal8 <- rbind(freqOptimal8,table)
}


plot(freqOptimal8[,2], type = 'l', col = 'red', ylim = c(13, 19))
lines(freqOptimal8[, 1])


