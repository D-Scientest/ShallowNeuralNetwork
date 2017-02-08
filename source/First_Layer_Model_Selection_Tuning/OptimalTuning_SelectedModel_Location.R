library(RStudioAMI)
passwd()
linkDropbox()
excludeSyncDropbox("*")
includeSyncDropbox("STAT640CompetitionPrivate")
install.packages("data.table");
install.packages("reshape2");install.packages("glmnet");
install.packages("doSNOW"); install.packages("foreach");
install.packages("ncvreg")

setwd("/home/rstudio/Dropbox/STAT640CompetitionPrivate")
library(data.table); library(doSNOW); library(foreach)
library(glmnet); library(ncvreg)


#########
# Optimal Tunning Location
source("source/First_Layer_Model_Selection_Tuning_Prediction/CVOptimalTuning_Location.R")
freqModel_activated_path <- paste0("data/ModelDecision/nonLinearSpline/activated_AllFreq_Location.csv")
freqModel_activated_loc <- read.table(freqModel_activated_path, sep = ',')
activated_AllFreq_Loc_Tuning <- matrix(0, nrow = nrow(freqModel_activated_loc), ncol = ncol(freqModel_activated_loc))
locLag <- 0;

stateLoc <- "activated" 

for(freqIdx_ in 1:1){
  for(locIdx_ in 1:70){
    activated_AllFreq_Loc_Tuning[locIdx_, freqIdx_] <- CVOptimalTuning_Location(locIdx_ = locIdx_, freqIdx_ = freqIdx_, state = stateLoc, locLag = locLag, model = freqModel_activated_loc[locIdx_, freqIdx_])
  }
}
activated_AllFreq_Loc_TuningPath <-  paste0("data/ModelDecision/nonLinearSpline/activated_AllFreq_Loc_TuningResult.csv")
write.table(activated_AllFreq_Loc_Tuning, activated_AllFreq_Loc_TuningPath, col.names = F, row.names = F, sep = ',')


freqModel_inactivated_path <- paste0("data/ModelDecision/nonLinearSpline/inhabited_AllFreq_Location.csv")
freqModel_inactivated_loc <- read.table(freqModel_inactivated_path, sep = ',')
inhabited_AllFreq_Loc_Tuning <- matrix(0, nrow = nrow(freqModel_inactivated_loc), ncol = ncol(freqModel_inactivated_loc))
stateBand <- "inhabited"
for(freqIdx_ in 1:1){
  for(locIdx_ in 1:70){
    print(locIdx_)
    inhabited_AllFreq_Loc_Tuning[locIdx_, freqIdx_] <- CVOptimalTuning_Location(locIdx_ = locIdx_, freqIdx_ = freqIdx_, state = stateLoc, locLag = locLag, model = freqModel_inactivated_loc[locIdx_, freqIdx_])
  }
}
inhabited_AllFreq_Loc_TuningPath <-  paste0("data/ModelDecision/nonLinearSpline/inhabited_AllFreq_Loc_TuningResult.csv")
write.table(inhabited_AllFreq_Loc_Tuning, inhabited_AllFreq_Loc_TuningPath, col.names = F, row.names = F, sep = ',')


