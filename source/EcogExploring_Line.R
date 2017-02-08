###############################################
##
## note:
##  input: senIdx: sentence index
##         freqIdx: frequency band index
##         timePoint: time point in each frequency of sentence
##         varIdx: variable index in each frequency band(1 to freqLength_)
##  Global necessary input: input_, breakPoint_, lengend
##
##  Vertical visualization:
##              freqIdx(varIdx): only freqIdx, whole frequency band will be ploted;
##                              freqIdx + varIdx, selected variables will be printed in each frequency band
##              freqLength: x:70, y:1
##                          
##              senIdx_: selected sentence will be printed in order with freqIdx(varIdx) information
##              direction: "vertical"
##  Horizontal visualization:
##              senIdx_(timePoint_): only senIdx_, whole sentence with all the time points will be ploted;
##                                   senIdx_ + timePoint_, selected time point in each sentence will be printed
##              direction: "horizontal"
###############################################

EcogExploring_Line <- function(input_, breakPoint_, senIdx_, freqIdx_ = NULL, freqLength_ = NULL,
                               timePoint_ = NULL, varIdx_ = NULL, direction_, legend = FALSE){
  breakPoint_ <- c(0, breakPoint_$V1)
  
  if(direction_ == "vertical"){
    # Retrieve the variable index in the selected frequency band
    if(!is.null(freqIdx_) & !is.null(varIdx_)){
      selectedVarIdx <- NULL
      for(i in 1:length(freqIdx_)){
        varStart <- freqLength_ * (freqIdx_[i]-1) + 1
        selectedVarIdx <- c(selectedVarIdx, varStart + varIdx_)
      }
      varIdx_ <- selectedVarIdx
    }else if(!is.null(freqIdx_)){
      varIdx_ <- NULL
      for(i in 1:length(freqIdx_)){
        varStart <-freqLength_ * (freqIdx_[i]-1) + 1 ;varEnd <- freqLength_ * freqIdx_[i]
        varIdx_ <- c(varIdx_, varStart:varEnd)
      }
    }else{
      print('Vertical: Require necessary parameters freqIdx_(frequency band Index)')
    }
    # Draw one sentence block at a time vertically with selected variables
    for(i in 1:length(senIdx_)){
      startBreakPoint <- breakPoint_[senIdx_[i]] + 1
      endBreakPoint <- breakPoint_[senIdx_[i]+1]
      verticalGraph <- input_ %>%
        select_(.dots = do.call(paste0, list(rep("V", length.out = length(varIdx_)), varIdx_))) %>%
        filter(as.numeric(rownames(.)) %in% startBreakPoint:endBreakPoint)%>%
        mutate(idx = 1:nrow(.)) %>%
        melt(., id.vars = "idx") %>%
        qplot(idx, value, data = ., colour = variable, geom = 'line')
      if(legend){
        return(verticalGraph)
      }else{
        return(verticalGraph + theme(legend.position="none"))
      }
    }
  }else if(direction_ == "horizontal"){
    # Retrieve the variable index in the selected frequency band
    if(!is.null(freqIdx_)){
      varIdx_ <- NULL
      for(i in 1:length(freqIdx_)){
        varStart <-freqLength_ * (freqIdx_[i]-1) + 1 ;varEnd <- freqLength_ * freqIdx_[i]
        varIdx_ <- c(varIdx_, varStart:varEnd)
      }
    }else{
      varIdx_ <- 1:ncol(input_)
    }
    # Retrieve time point index in the selected senetnece
    if(!is.null(senIdx_) & !is.null(timePoint_)){
      selectedTimePoint <- NULL
      for(i in 1:length(senIdx_)){
        startBreakPoint <-as.numeric(breakPoint_[senIdx_[i]] + 1) 
        selectedTimePoint <- c(selectedTimePoint, startBreakPoint + timePoint_)
      }
      timePoint_ <- selectedTimePoint
    }else if(!is.null(senIdx_)){
      timePoint_ <- NULL
      for(i in 1:length(senIdx_)){
        startBreakPoint <-as.numeric(breakPoint_[senIdx_[i]] + 1) ;endBreakPoint <- as.numeric(breakPoint_[senIdx_[i] + 1])
        timePoint_ <- c(timePoint_, startBreakPoint:endBreakPoint)
      }
    }else{
      print("Horizontal: require nessacery parameter senIdx_(Sentence Index)")
    }
    
    horizontalGraph <- input_ %>%
      select_(.dots = as.list(do.call(paste0, list(rep("V", length.out = length(varIdx_)), varIdx_)))) %>%
      filter(as.numeric(rownames(.)) %in% timePoint_) %>%
      t(.) %>%
      melt(.) %>%
      qplot(Var1, value, data = ., group = factor(Var2), 
            colour = factor(Var2), geom = 'line')
    if(legend){
      return(horizontalGraph)
    }else{
      return(horizontalGraph + theme(legend.position="none"))
    }
    
  }else{
    print("Please Enter legitimate direction: vertical/horizontal")
  }
  
}