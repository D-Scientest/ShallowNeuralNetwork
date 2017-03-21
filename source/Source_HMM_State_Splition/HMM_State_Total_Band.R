#########################################################
#
# HMM data splition: 
#           Helper function for HMM_State_output_Band.R
#########################################################

HMM_State_Total_Band <- function(input_, bandIdx_, breakPoint_, output_ = NULL, senNumber_, seed_ = 1234){
  source("source/Source_HMM_State_Splition/HMM_State_Band.R")
  inhabitedState <- NULL
  activatedState <- NULL
  inhabited_breakPoint <- NULL
  activated_breakPoint <- NULL
  
  for(senIdx_ in 1:senNumber_){
    senState <- HMM_State_Band(input_ = input_, senIdx_ = senIdx_, bandIdx_ = bandIdx_, 
                          breakPoint_ = train_breakPoint, seed_ = seed_)
    inhabitedState <- c(inhabitedState, senState$inhabitated)
    activatedState <- c(activatedState, senState$activated)
    
    inhabited_breakPoint <- c(inhabited_breakPoint, length(senState$inhabitated))
    activated_breakPoint <- c(activated_breakPoint, length(senState$activated))
  }
  
  startBand <- 70*(bandIdx_-1) + 1; endBand <- 70*bandIdx_;
  if(!is.null(output_)){
    statePeriod <- list(inhabitedPeriod_X = input_[inhabitedState,startBand:endBand],
                        activatedPeriod_X = input_[activatedState,startBand:endBand],
                        inhabitedPeriod_Y = output_[inhabitedState, 1:8], 
                        activatedPeriod_Y = output_[activatedState,1:8],
                        inhabitedPeriod_breakPoint = cumsum(inhabited_breakPoint), 
                        activatedPeriod_breakPoint = cumsum(activated_breakPoint),
                        inhabitedIdx_X = inhabitedState,
                        activatedIdx_X = activatedState)
  }else{
    statePeriod <- list(inhabitedPeriod_X = input_[inhabitedState,startBand:endBand],
                        activatedPeriod_X = input_[activatedState,startBand:endBand],
                        inhabitedPeriod_breakPoint = cumsum(inhabited_breakPoint), 
                        activatedPeriod_breakPoint = cumsum(activated_breakPoint),
                        inhabitedIdx_X = inhabitedState,
                        activatedIdx_X = activatedState)
  }
  
  return(statePeriod)
}




