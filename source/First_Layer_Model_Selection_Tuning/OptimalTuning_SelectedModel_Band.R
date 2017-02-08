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
# Optimal Tunning Band
source("source/CVOptimalTuning_Band.R")
# freqModel_activated_path <- paste0("data/ModelDecision/activated_AllFreq_Band.csv")
# freqModel_activated_bands <- read.table(freqModel_activated_path, sep = ',')
freqModel_inactivated_path <- paste0("data/ModelDecision/inhabited_AllFreq_Band.csv")
freqModel_inactivated_bands <- read.table(freqModel_inactivated_path, sep = ',')
# activated_AllFreq_Band_Tuning <- matrix(0, nrow = nrow(freqModel_activated_bands), ncol = ncol(freqModel_activated_bands))
inhabited_AllFreq_Band_Tuning <- matrix(0, nrow = nrow(freqModel_inactivated_bands), ncol = ncol(freqModel_inactivated_bands))
bandLag <- 2;

# stateBand <- "activated"
# for(freqIdx_ in 1:8){
#   for(bandIdx_ in 1:6){
#     activated_AllFreq_Band_Tuning[bandIdx_, freqIdx_] <- CVOptimalTuning_Band(bandIdx_ = bandIdx_, freqIdx_ = freqIdx_, state = stateBand, bandLag = bandLag, model = freqModel_inactivated_path[bandIdx_, freqIdx_])
#   }
# }
# activated_AllFreq_Band_TuningPath <-  paste0("data/ModelDecision/activated_AllFreq_Band_TuningResult.csv")
# write.table(activated_AllFreq_Band_Tuning, activated_AllFreq_Band_TuningPath, col.names = F, row.names = F, sep = ',')


stateBand <- "inhabited"
# for(freqIdx_ in 1:8){
#   for(bandIdx_ in 5:6){
#     inhabited_AllFreq_Band_Tuning[bandIdx_, freqIdx_] <- CVOptimalTuning_Band(bandIdx_ = bandIdx_, freqIdx_ = freqIdx_, state = stateBand, bandLag = bandLag, model = freqModel_inactivated_bands[bandIdx_, freqIdx_])
#   }
# }
# manual tunning
bandIdx_ <- 6
for(freqIdx_ in 1:8){
  inhabited_AllFreq_Band_Tuning[bandIdx_, freqIdx_] <- CVOptimalTuning_Band(bandIdx_ = bandIdx_, freqIdx_ = freqIdx_, state = stateBand, bandLag = bandLag, model = freqModel_inactivated_bands[bandIdx_, freqIdx_])
}
bandIdx_ <- 5
for(freqIdx_ in c(5, 7)){
  inhabited_AllFreq_Band_Tuning[bandIdx_, freqIdx_] <- CVOptimalTuning_Band(bandIdx_ = bandIdx_, freqIdx_ = freqIdx_, state = stateBand, bandLag = bandLag, model = freqModel_inactivated_bands[bandIdx_, freqIdx_])
}
# inhabited
inhabited_AllFreq_Band_TuningPath <-  paste0("data/ModelDecision/inhabited_AllFreq_Band_TuningResult.csv")
write.table(inhabited_AllFreq_Band_Tuning, inhabited_AllFreq_Band_TuningPath, col.names = F, row.names = F, sep = ',')







