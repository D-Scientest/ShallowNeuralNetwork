###################################################
#
# Create frequency band new training input variable
#
###################################################

Optimal_Model_activated <- read.table('data/ModelDecision/OLS/activated_AllFreq_Band_OLS.csv', sep = ',')
Optimal_model_inhabited <- read.table('data/ModelDecision/OLS/inhabited_AllFreq_Band_OLS.csv', sep = ',')

Optimal_Parameter_activated <- read.table('data/ModelDecision/OLS/activated_AllFreq_Band_TuningResult_OLS.csv', sep = ',')
Optimal_Parameter_inhabited <- read.table('data/ModelDecision/OLS/inhabited_AllFreq_Band_TuningResult_OLS.csv', sep = ',')

source('source/Source_First_Layer_Model_Selection_Tuning/Prediction_ParameterEstimation_Band_Train.R')
stateList <- c('activated','inhabited')

core <- parallel:::detectCores(); cl <- makeCluster((core - 1))
registerDoSNOW(cl)
pb <- txtProgressBar(max = 8, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

foreach(freqIdx_=1:8, .options.snow = opts, .packages = c('glmnet', 'data.table')) %dopar% {
  NewInput <- matrix(0, nrow = 3055, ncol = 0)
  for(bandIdx_ in 1:6){
    stateNewInput <- NULL
    activatedIdxPath <- paste0('data/HMM_State_Train/activatedIdx_',bandIdx_,'.csv')
    activatedIdx <- read.table(activatedIdxPath, sep = ',')
    inhibitedIdxPath <- paste0('data/HMM_State_Train/inhabitedIdx_',bandIdx_,'.csv')
    inhibitedIdx <- read.table(inhibitedIdxPath, sep = ',')
    stateNewInputIdx <- rbind(activatedIdx, inhibitedIdx)
    for(state_ in 1:2){
      if(state_ == 1){
        predictedValue <- as.data.frame(Prediction_ParameterEstimation_Band_Train(freqIdx_ = freqIdx_, bandIdx_ = bandIdx_, state = stateList[state_], bandLag = 2, model = Optimal_Model_activated[bandIdx_,freqIdx_], optimalTuning = Optimal_Parameter_activated[bandIdx_,freqIdx_]))
        stateNewInput <- rbind(stateNewInput, predictedValue)
      }else if(state_ == 2){
        predictedValue <- as.data.frame(Prediction_ParameterEstimation_Band_Train(freqIdx_ = freqIdx_, bandIdx_ = bandIdx_, state = stateList[state_], bandLag = 2, model = Optimal_model_inhabited[bandIdx_,freqIdx_], optimalTuning = Optimal_Parameter_inhabited[bandIdx_,freqIdx_]))
        colnames(predictedValue) <- colnames(stateNewInput)
        stateNewInput <- rbind(stateNewInput, predictedValue)
      }
    }
    stateNewInput <- cbind(stateNewInputIdx, stateNewInput)
    stateNewInput_sorted <- stateNewInput[order(stateNewInput[,1]),]
    NewInput <- cbind(NewInput, stateNewInput_sorted[,-1])
  }
  NewInputPath <-  paste0("data/NewVariable/OLS/Train/Freq",freqIdx_,"_Band.csv")
  write.table(NewInput, NewInputPath, col.names = F, row.names = F, sep = ',')
}
stopCluster(cl)
close(pb)