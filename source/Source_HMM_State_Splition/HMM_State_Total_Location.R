# setwd('/home/zhenshan/Dropbox/STAT640CompetitionPrivate')
HMM_State_Total_Location <- function(input_, LocIdx_, breakPoint_, output_ = NULL, senNumber_ = 140, seed_ = 1234){
  # input_<- train_x_switch; LocIdx_ <- 1; breakPoint_ <- train_breakPoint; senNumber <- 2;
  # output_ <- train_y
  source("source/Source_HMM_State_Splition/HMM_State_Location.R")
  inhabitedState <- NULL
  activatedState <- NULL
  inhabited_breakPoint <- NULL
  activated_breakPoint <- NULL
  
  for(senIdx_ in 1:senNumber_){
    senState <- HMM_State_Location(input_ = input_, senIdx_ = senIdx_, locIdx_ = LocIdx_, 
                          breakPoint_ = train_breakPoint, seed_ = seed_)
    inhabitedState <- c(inhabitedState, senState$inhabitated)
    activatedState <- c(activatedState, senState$activated)
    
    inhabited_breakPoint <- c(inhabited_breakPoint, length(senState$inhabitated))
    activated_breakPoint <- c(activated_breakPoint, length(senState$activated))
  }
  
  startLoc <- 6*(LocIdx_-1) + 1; endLoc <- 6*LocIdx_;
  if(!is.null(output_)){
    statePeriod <- list(inhabitedPeriod_X = input_[inhabitedState,startLoc:endLoc],
                        activatedPeriod_X = input_[activatedState,startLoc:endLoc],
                        inhabitedPeriod_Y = output_[inhabitedState, 1:8], 
                        activatedPeriod_Y = output_[activatedState,1:8],
                        inhabitedPeriod_breakPoint = cumsum(inhabited_breakPoint), 
                        activatedPeriod_breakPoint = cumsum(activated_breakPoint),
                        inhabitedIdx_X = inhabitedState,
                        activatedIdx_X = activatedState)
  }else{
    statePeriod <- list(inhabitedPeriod_X = input_[inhabitedState,startLoc:endLoc],
                        activatedPeriod_X = input_[activatedState,startLoc:endLoc],
                        inhabitedPeriod_breakPoint = cumsum(inhabited_breakPoint), 
                        activatedPeriod_breakPoint = cumsum(activated_breakPoint),
                        inhabitedIdx_X = inhabitedState,
                        activatedIdx_X = activatedState)
  }
  
  return(statePeriod)
}




