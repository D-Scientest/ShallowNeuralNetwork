###################################################
#
# Create electronode location new testing input variable
#
###################################################
Optimal_Model_activated <- read.table('data/ModelDecision/OLS/activated_AllFreq_Location_OLS.csv', sep = ',')
Optimal_model_inhabited <- read.table('data/ModelDecision/OLS/inhabited_AllFreq_Location_OLS.csv', sep = ',')

Optimal_Parameter_activated <- read.table('data/ModelDecision/OLS/activated_AllFreq_Loc_TuningResult_OLS.csv', sep = ',')
Optimal_Parameter_inhabited <- read.table('data/ModelDecision/OLS/inhabited_AllFreq_Loc_TuningResult_OLS.csv', sep = ',')

source('source/Source_First_Layer_Model_Selection_Tuning/Prediction_ParameterEstimation_Location.R')
stateList <- c('activated','inhabited')

core <- parallel:::detectCores(); cl <- makeCluster((core - 1))
registerDoSNOW(cl)
pb <- txtProgressBar(max = 8, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

foreach(freqIdx_=1:8, .options.snow = opts, .packages = c('glmnet', 'data.table')) %dopar% {
  NewInput <- matrix(0, nrow = 1554, ncol = 0)
  for(locIdx_ in 1:70){
    stateNewInput <- NULL
    activatedIdxPath <- paste0('data/HMM_State_Location_Test/activatedIdx_',locIdx_,'.csv')
    activatedIdx <- read.table(activatedIdxPath, sep = ',')
    inhibitedIdxPath <- paste0('data/HMM_State_Location_Test/inhabitedIdx_',locIdx_,'.csv')
    inhibitedIdx <- read.table(inhibitedIdxPath, sep = ',')
    stateNewInputIdx <- rbind(activatedIdx, inhibitedIdx)
    for(state_ in 1:2){
      if(state_ == 1){
        predictedValue <- as.data.frame(Prediction_ParameterEstimation_Location(freqIdx_ = freqIdx_, locIdx_ = locIdx_, state = stateList[state_], locLag = 2, model = Optimal_Model_activated[locIdx_,freqIdx_], optimalTuning = Optimal_Parameter_activated[locIdx_,freqIdx_]))
        stateNewInput <- rbind(stateNewInput, predictedValue)
      }else if(state_ == 2){
        predictedValue <- as.data.frame(Prediction_ParameterEstimation_Location(freqIdx_ = freqIdx_, locIdx_ = locIdx_, state = stateList[state_], locLag = 2, model = Optimal_model_inhabited[locIdx_,freqIdx_], optimalTuning = Optimal_Parameter_inhabited[locIdx_,freqIdx_]))
        colnames(predictedValue) <- colnames(stateNewInput)
        stateNewInput <- rbind(stateNewInput, predictedValue)
      }
    }
    stateNewInput <- cbind(stateNewInputIdx, stateNewInput)
    stateNewInput_sorted <- stateNewInput[order(stateNewInput[,1]),]
    NewInput <- cbind(NewInput, stateNewInput_sorted[,-1])
  }
  NewInputPath <-  paste0("data/NewVariable/OLS/Test/Freq",freqIdx_,"_Location.csv")
  write.table(NewInput, NewInputPath, col.names = F, row.names = F, sep = ',')
}
stopCluster(cl)
close(pb)