#########################################################
#
# HMM data splition: 
#           Helper function for HMM_State_Total.R
#########################################################

HMM_State_Band <- function(input_, senIdx_, bandIdx_, breakPoint_, seed_ = 1234, plot_ = FALSE){
  # Create the formula list for HMM mdoel
  F_list <- list()
  for(locIdx in 1:70){
    locN_ <- 70*(bandIdx_-1) + locIdx
    F_list[[locIdx]] <- as.formula(paste0('V',locN_, "~", 1))
  }
  
  # Create family list for HMM model
  fam_list <- list()
  for(locIdx in 1:70){
    fam_list[[locIdx]] <- gaussian()
  }
  
  # Index parameters for bands and sentence
  breakPoint_ <- rbind(0,breakPoint_)
  start <- breakPoint_[senIdx_,] + 1; end <- breakPoint_[senIdx_ + 1,]
  startBand <- 70*(bandIdx_-1) + 1; endBand <- 70*bandIdx_;
  
  ModelData <- input_[start:end, startBand:endBand]; #Y_ <- train_y[start:end, 1:32]
  set.seed(seed_)
  HMM<-depmix(F_list,data=ModelData,nstates=2,family=fam_list,  transition = ~1) #We’re setting the X_ and Y_ as our response variables, using the data frame we just built, want to set 3 different regimes, and setting the response distributions to be gaussian.
  HMMfit<-fit(HMM, verbose = FALSE) #fit our model to the data set
  HMMpost<-posterior(HMMfit) #find the posterior odds for each state over our data set
  stateList <- list(inhabitated = which(HMMpost$state == 1) - 1 + start,
                    activated = which(HMMpost$state == 2) - 1 + start)
  
  if(plot_){
    library(ggplot2); library(gridExtra)
    DateTS <- start:end
    DFIndicators <- data.frame(DateTS, train_y_mean$V1[start:end]); 
    DFIndicatorsClean <- DFIndicators
    
    Plot1Data<-data.frame(DFIndicatorsClean, HMMpost$state)#, HMMpost_y$state)
    X_Plot<-ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,2]))+geom_line(color="darkblue")+labs(title="Log Returns",y="Log Returns",x="Date")
    Y_Plot<-ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,3]))+geom_line(color="darkblue")+labs(title="Log Returns",y="Log Returns",x="Date")
    # RegimePlot<-ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,4]))+geom_line(color="darkblue")+labs(title="Log Returns",y="Log Returns",x="Date")
    grid.arrange(X_Plot,Y_Plot,ncol=1,nrow=2)
    
    RegimePlotData<-data.frame(Plot1Data$DateTS,HMMpost)
    Regime1Plot<-ggplot(RegimePlotData,aes(x=RegimePlotData[,1],y=RegimePlotData[,3]))+geom_line(color="purple")+labs(title="Regime 1",y="Probability",x="Date")
    Regime2Plot<-ggplot(RegimePlotData,aes(x=RegimePlotData[,1],y=RegimePlotData[,4]))+geom_line(color="purple")+labs(title="Regime 1",y="Probability",x="Date")
    # Regime3Plot<-ggplot(RegimePlotData,aes(x=RegimePlotData[,1],y=RegimePlotData[,5]))+geom_line(color="purple")+labs(title="Regime 1",y="Probability",x="Date")
    grid.arrange(Regime1Plot,Regime2Plot,ncol=1,nrow=2)
  }
  return(stateList)
}
