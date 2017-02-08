########################################################
#
# Electronode location optimal model selection
#     Running environment: Amazon Web Service
#     Tutorial: http://www.louisaslett.com/RStudio_AMI/
#     Output: Query error for locations
#
########################################################

library(RStudioAMI)
passwd()
linkDropbox()
excludeSyncDropbox("*")
includeSyncDropbox("STAT640Competition")
install.packages("data.table");
install.packages("reshape2");install.packages("glmnet");
install.packages("doSNOW"); install.packages("foreach");
install.packages("ncvreg")

setwd("/path/to/AWS/directory")
library(data.table); library(doSNOW); library(foreach)
library(glmnet); library(ncvreg)
source('source/Source_First_Layer_Model_Selection_Tuning/HMM_State_Model_Selection_Location.R')

stateList <- c("activated","inhabited"); locList <- 1:70
freqList <- 1:8; locLagList <- 0; queryNum <- 115 

stateModelComp_Location <- NULL
for(freq_ in freqList){
  for(state_ in 1:2){
    for(locIdx_ in locList){
      ModelComp_Location <- HMM_State_Model_Selection_Location(freqIdx_ = freq_, locIdx_ = locIdx_, 
                                                               state = stateList[state_], locLag = locLagList, queryNum = queryNum)
      ModelComp_Location <- c(state_, locIdx_, ModelComp_Location)
      names(ModelComp_Location)[1:2] <- c("state", "Location")
      stateModelComp_Location <- rbind(stateModelComp_Location, ModelComp_Location)
    }
  }
  stateModelComp_Location_path <- paste0("data/ModelDecision/stateModelComp_Location_freq",freq_,"_lag", locLagList, ".csv")
  write.table(stateModelComp_Location, stateModelComp_Location_path, col.names = FALSE, row.names = FALSE, sep = ',')
}


