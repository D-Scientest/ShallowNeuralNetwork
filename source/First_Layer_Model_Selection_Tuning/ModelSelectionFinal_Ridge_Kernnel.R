library(RStudioAMI)
passwd()
linkDropbox()
excludeSyncDropbox("*")
includeSyncDropbox("STAT640CompetitionPrivate")
install.packages("magrittr"); install.packages("data.table");
install.packages("reshape2");install.packages("glmnet");
install.packages("doSNOW"); install.packages("foreach");
install.packages('kernlab')

setwd("/home/rstudio/Dropbox/STAT640CompetitionPrivate")
library(magrittr);library(data.table);library(reshape2);
library(glmnet);library(doSNOW);library(foreach);library(kernlab)

source('source/HMM_State_Model_Selection.R')

stateList <- c("activated","inhabited"); bandList <- 1:6
freqList <- 6; bandLagList <- 2; queryNum <- 115 
bandList2 <- 2:6
# ## Initalize Parallel backend
# parallel:::detectCores()
# cl <- makeCluster(16) # change the number of cores in makeCluster
# ## Initialization process bar in computing process
# registerDoSNOW(cl)
# pb <- txtProgressBar(max = length(stateList) * length(bandList), style = 3)# max: the number of threads during the computing
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)

stateModelComp <- NULL
state_ <- 1
for(bandIdx_ in bandList2){
  modelComp <- HMM_State_Model_Selection(freqIdx_ = freqList, bandIdx_ = bandIdx_, 
                                         state = stateList[state_], bandLag = bandLagList, queryNum = queryNum)
  stateModelComp <- rbind(stateModelComp, modelComp)
  
  stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
  write.table(stateModelComp, stateModelComp_path, col.names = FALSE, row.names = FALSE, sep = ',')
  
}
# state_ <- 2
# for(bandIdx_ in bandList){
#   modelComp <- HMM_State_Model_Selection(freqIdx_ = freqList, bandIdx_ = bandIdx_, 
#                                          state = stateList[state_], bandLag = bandLagList, queryNum = queryNum)
#   stateModelComp <- rbind(stateModelComp, modelComp)
#   
#   stateModelComp_path <- paste0("data/output/stateModelComp_freq",freqList,"_lag",bandLagList,"_state",state_,"_bandIdx", bandIdx_,".csv")
#   write.table(stateModelComp, stateModelComp_path, col.names = FALSE, row.names = FALSE, sep = ',')
# }

# stateModelComp <- foreach(state_ = 1:2, .combine = 'cbind') %:%
#   foreach(bandIdx_ = bandList, .packages = c("glmnet", "foreach", "doSNOW", "kernlab", "data.table"), 
#           .combine='rbind') %do% { 
#             HMM_State_Model_Selection(freqIdx_ = freqList, bandIdx_ = bandIdx_, 
#                                       state = stateList[state_], bandLag = bandLagList, queryNum = queryNum)
#           }
# stopCluster(cl)
# close(pb)
