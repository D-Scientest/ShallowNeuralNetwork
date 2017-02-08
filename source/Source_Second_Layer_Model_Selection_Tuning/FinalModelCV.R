FinalModelCV <- function(inputTrain_, outputTrain_, inputTest_, break_point_, freqIdx_, realInputTest_ = NULL){
  # library(glmnet); library(foreach); library(doSNOW)
  # inputTrain_ <- NewInput_Train
  # outputTrain_ <- train_y[,freqIdxFull_]
  # inputTest_ <- NewInput_Test
  # break_point_ <- train_breakPoint
  # freqIdx_ <- freqIdxFull_
  # realInputTest_ <- as.data.frame(realTrain_y[,freqIdxFull_])
  # glmnet_model <- function(train_input ,train_output, 
  #                          val_input, val_output = NULL, 
  #                          lam, al = 0, test = FALSE){
  #   # train_input <- as.matrix(dat$trainX); train_output <- as.data.frame(dat$trainY)
  #   # val_input <- as.matrix(dat$validX); val_output<- as.data.frame(dat$validY)
  #   # lam <- 0;al <- 0
  #   if(!test){
  #     fitglm <- glmnet(x=train_input,y= as.matrix(train_output),family="gaussian",standardize = FALSE, lambda=lam,alpha=al)
  #     ptest_glm <- predict(fitglm, val_input)
  #     mse_glm <- apply(as.matrix(ptest_glm), 2, MSE, predY = val_output)
  #     return(mse_glm)
  #   }else{
  #     fitglm <- glmnet(x=train_input,y= as.matrix(train_output),family="gaussian",standardize = FALSE, lambda=lam,alpha=al)
  #     ptest_glm <- predict(fitglm, val_input)
  #     return(ptest_glm)
  #   }
  # }
  # 
  MSE = function(Y, predY) {sqrt(mean((Y - predY)^2))} ## Compute MSE
  # ## initialize parameters for k-fold parameter tuning
  # lamRidge = exp(seq(-8,8, length.out = 100));
  # cv_times_ridge = 10; 
  # 
  # core <- parallel:::detectCores(); 
  # cl <- makeCluster((core-1))
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = cv_times_ridge, style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  source('source/splitBlocksCV.R', local = TRUE)
  # ## ridge in a parallel loop
  # ridgeLoop <- foreach(i=1:cv_times_ridge, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
  #   ## split data for cv
    dat <- splitBlocksCV(X = inputTrain_, Y = as.data.frame(outputTrain_), realY = as.data.frame(realInputTest_),break_point = break_point_, seed = i)
  #   
  #   ## training data
  #   ridgeFitMse <- glmnet_model(train_input = as.matrix(dat$trainX), 
  #                               train_output = as.data.frame(dat$trainY),
  #                               val_input = as.matrix(dat$validX), 
  #                               val_output = as.data.frame(dat$realValidY),
  #                               lam = lamRidge)
  #   ridgeFitMse
  # }
  # stopCluster(cl)
  # close(pb)
  # 
  # # find the optimal tunnning parameters
  # # library(Hmisc)
  # ridgeExpectedMSE <-apply(ridgeLoop, 1, mean)
  # # plot(ridgeExpectedMSE, type = 'l')
  # # ridgeSE = sqrt(apply(ridgeLoop,1,var)/cv_times)
  # # errbar(1:length(lamRidge),ridgeExpectedMSE,ridgeExpectedMSE+ridgeSE,ridgeExpectedMSE-ridgeSE,add=TRUE)
  # optimalRidge <- lamRidge[which.min(ridgeExpectedMSE)]
  # 
  # mseOutputPath <- paste0("data/output/smooth/freq_",freqIdx_,'.csv')
  # write.table(min(ridgeExpectedMSE), mseOutputPath, col.names = FALSE, row.names = FALSE, sep = ',')
  # 
  # 
  # ridgePred <- glmnet_model(train_input = as.matrix(inputTrain_),
  #                             train_output = as.data.frame(outputTrain_),
  #                             val_input = as.matrix(inputTest_),
  #                             lam = optimalRidge, test = TRUE)
  # 
  datC <- cbind(dat$trainY,dat$trainX)
  colnames(datC)[1:7] <- c("Y", paste0(1:6, 'VV'))
  olsFit <- lm(Y~.,data = datC)
  colnames(dat$validX)[1:6] <- paste0(1:6, 'VV')
  olsFitMSE <- MSE(Y = dat$validY,predY = predict(olsFit, dat$validX))
  
  mseOutputPath <- paste0("data/Prediction/OLS/freq_",freqIdx_,'.csv')
  write.table(olsFitMSE, mseOutputPath, col.names = FALSE, row.names = FALSE, sep = ',')
}