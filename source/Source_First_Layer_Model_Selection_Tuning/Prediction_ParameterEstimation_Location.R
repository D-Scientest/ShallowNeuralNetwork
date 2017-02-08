# setwd('/home/zhenshan/Dropbox/STAT640CompetitionPrivate')
Prediction_ParameterEstimation_Location <- function(freqIdx_, locIdx_, state, locLag, model, optimalTuning = NULL){
  ##################
  ## Import data
  ##################
  state_x_path <- paste0("data/HMM_State_Location_Train/", state, "_X_",locIdx_,"_Location.csv")
  state_y_path <- paste0("data/HMM_State_Location_Train/", state, "_Y_",locIdx_,"_Location.csv")
  state_breakPoint_path <- paste0("data/HMM_State_Location_Train/", state, "Period_breakPoint_",locIdx_,"_Location.csv")
  
  state_x_test_path <- paste0("data/HMM_State_Location_Test/", state, "_X_",locIdx_,"_Location.csv")
  # state_y_test_path <- paste0("data/HMM_State_Test/", state, "_Y_",bandIdx_,"_Location.csv")
  state_test_breakPoint_path <- paste0("data/HMM_State_Location_Test/", state, "Period_breakPoint_",locIdx_,"_Location.csv")
  
  state_x <- as.data.frame(fread(state_x_path))
  state_y <- as.data.frame(fread(state_y_path))[,freqIdx_]
  state_breakPoint <- as.data.frame(fread(state_breakPoint_path))
  state_breakPoint_ <- rbind(0,state_breakPoint)
  
  state_x_test <- as.data.frame(fread(state_x_test_path))
  # state_y_test <- as.data.frame(fread(state_y_test_path))[,freqIdx_]
  state_breakPoint_test <- as.data.frame(fread(state_test_breakPoint_path))
  state_breakPoint_test_ <- rbind(0,state_breakPoint_test)
  
  # Helper function
  MSE = function(Y, predY) {sqrt(mean((Y - predY)^2))} ## Compute MSE
  
  
  # #################
  # # Create lag matrix for band
  # Rarray <- foreach(senIdx = 1:140, .combine = 'rbind') %do% {
  #   # Each sentence with removing first rows and make up last rows
  #   start <- state_breakPoint_[senIdx, ] + 1; end <- state_breakPoint_[senIdx + 1,]
  #   trains_x <- data.frame(state_x[start:end, ])
  #   RarraySent <- matrix(0,nrow = (end-start + 1), ncol = 1)
  #   RarraySent_Loc <- trains_x
  #   for(i in 1:locLag){
  #     Rr <- trains_x[-(1:i), ]
  #     imputed <- mean(as.numeric(RarraySent_Loc[nrow(RarraySent_Loc)-i + 1,]))
  #     makeUp <- matrix(imputed, nrow = i, ncol = 6)
  #     colnames(makeUp) <- colnames(Rr)
  #     Rm <- rbind(Rr, makeUp)
  #     RarraySent_Loc <- cbind(RarraySent_Loc, Rm)
  #   }
  #   RarraySent <- cbind(RarraySent, RarraySent_Loc)
  #   RarraySent[,-1]
  # }
  # 
  # #######
  # # Testing Data
  # RarrayTest <- foreach(senIdx_test = 1:70, .combine = 'rbind') %do% {
  #   # Each sentence with removing first rows and make up last rows
  #   startTest <- state_breakPoint_test_[senIdx_test, ] + 1; endTest <- state_breakPoint_test_[senIdx_test + 1,]
  #   trains_x_test <- data.frame(state_x_test[startTest:endTest, ])
  #   RarraySentTest <- matrix(0,nrow = (endTest-startTest + 1), ncol = 1)
  #   RarraySentTest_Band <- trains_x_test
  #   for(i in 1:bandLag){
  #     RrTest <- trains_x_test[-(1:i), ]
  #     imputedTest <- mean(as.numeric(RarraySentTest_Band[nrow(RarraySentTest_Band)-i + 1,]))
  #     makeUpTest <- matrix(imputedTest, nrow = i, ncol = 6)
  #     colnames(makeUpTest) <- colnames(RrTest)
  #     RmTest <- rbind(RrTest, makeUpTest)
  #     RarraySentTest_Band <- cbind(RarraySentTest_Band, RmTest)
  #   }
  #   RarraySentTest <- cbind(RarraySentTest, RarraySentTest_Band)
  #   RarraySentTest[,-1]
  # }  
  # 
  #############
  #
  # Fitting models
  
  # library(glmnet)
  # glmnet_model <- function(train_input ,train_output, 
  #                          val_input, val_output = NULL, 
  #                          lam, al = 0, panelty_factor = NULL, test = FALSE){
  #   # train_input <- as.matrix(dat$trainX); train_output <- as.data.frame(dat$trainY)
  #   # val_input <- as.matrix(dat$validX); val_output<- as.data.frame(dat$validY)
  #   # lam <- lamRidge;al <- 0.2 ; panelty_factor <- 1/abs(betals)
  #   if(!test){
  #     if(is.null(panelty_factor)){
  #       fitglm <- glmnet(x=train_input,y= as.matrix(train_output),
  #                        family="gaussian",standardize = FALSE, 
  #                        lambda=lam,alpha=al)
  #     }else{
  #       fitglm <- glmnet(x=train_input,y= as.matrix(train_output),
  #                        family="gaussian",standardize = FALSE, 
  #                        lambda=lam,alpha=al, penalty.factor = panelty_factor)
  #     }
  #     ptest_glm <- predict(fitglm, val_input)
  #     mse_glm <- apply(as.matrix(ptest_glm), 2, MSE, predY = val_output)
  #     return(mse_glm)
  #   }else{
  #     if(is.null(panelty_factor)){
  #       fitglm <- glmnet(x=train_input,y= as.matrix(train_output),
  #                        family="gaussian",standardize = FALSE, 
  #                        lambda=lam,alpha=al)
  #     }else{
  #       # train_input = as.matrix(state_x);
  #       # train_output = as.data.frame(state_y);
  #       # val_input = as.matrix(state_x_test);
  #       # lam = 0; al = 1;  panelty_factor = 1/abs(betals); test = TRUE
  #       # 
  #       fitglm <- glmnet(x=train_input,y= as.matrix(train_output),
  #                        family="gaussian",standardize = FALSE, 
  #                        lambda=lam,alpha=al, penalty.factor = panelty_factor)
  #     }
  #     ptest_glm <- predict(fitglm, val_input)
  #     return(ptest_glm)
  #   }
  # }
  # 
  # library(ncvreg)
  # ncvreg_model <- function(train_input ,train_output, 
  #                          val_input, val_output = NULL, 
  #                          lam, panelt, test = FALSE){
  #   # train_input <- as.matrix(dat$trainX); train_output <- dat$trainY
  #   # val_input <- as.matrix(dat$validX); val_output<- dat$validY
  #   # lam <- lambdaMCp;panelt <- "MCP"
  #   if(!test){
  #     fitncv <- ncvreg(train_input,train_output,family="gaussian",lambda=lam, penalty= panelt)
  #     ptest_ncv <- predict(fitncv, val_input)
  #     mse_ncv <- apply(as.matrix(ptest_ncv), 2, MSE, predY = val_output)
  #     return(mse_ncv)
  #   }else{
  #     fitncv <- ncvreg(train_input,train_output,family="gaussian",lambda=lam, penalty= panelt)
  #     ptest_ncv <- predict(fitncv, val_input)
  #     return(ptest_ncv)
  #   }
  # }
  
  if(model == "OLS"){
    ## training data
    datC <- cbind(state_y, state_x)
    colnames(datC)[1] <- "Y"
    olsFit <- lm(Y~.,data = datC)
    olsPred <- predict(olsFit, state_x_test)
    
    return(olsPred)
  }else if(model == "Ridge"){
    ## training data
    ridgePred <- glmnet_model(train_input = as.matrix(state_x), 
                              train_output = as.data.frame(state_y),
                              val_input = as.matrix(state_x_test), 
                              lam = optimalTuning, test = TRUE)

    return(ridgePred)
  }else if(model == 'Adaptive L'){
    betals = solve(t(as.matrix(state_x))%*%as.matrix(state_x))%*%t(as.matrix(state_x))%*%state_y
    AdLPred <- glmnet_model(train_input = as.matrix(state_x), 
                            train_output = as.data.frame(state_y),
                            val_input = as.matrix(state_x_test), 
                            lam = optimalTuning, al = 1,  panelty_factor = 1/abs(betals), test = TRUE)
    return(AdLPred)
  }else if(model == 'MC+'){
    paneltyType <- 'MCP' 
    mcpFred <- ncvreg_model(train_input = as.matrix(state_x), 
                              train_output = state_y,
                              val_input = as.matrix(state_x_test), 
                              lam = optimalTuning,panelt = paneltyType, test = TRUE)
 
    return(mcpFred)
  }else if(model == 'SCAD'){
    paneltyType_scad <- 'SCAD' 
    
    ## training data
    scadFred <- ncvreg_model(train_input = as.matrix(state_x), 
                               train_output = state_y,
                               val_input = as.matrix(state_x_test), 
                              lam = optimalTuning, panelt = paneltyType_scad, test = TRUE)

    return(scadFred)
  }
  
  # ########
  # # Elastic Net
  # ########
  # ## initialize parameters for k-fold parameter tuning
  # lamEN = exp(seq(-8,8, length.out = 100)); cv_times_EN = 100; 
  # alEN <- c(0,0.2,0.4,0.6,0.8,1); tunParamEN <- expand.grid(lamEN, alEN)
  # 
  # core <- parallel:::detectCores(); cl <- makeCluster((core-1))
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = nrow(tunParamEN), style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # source('splitBlocksCV.R', local = TRUE)
  # ## ridge in a parallel loop
  # ENLoop <- foreach(alpha_ = 1:length(alEN), .combine = 'rbind') %:%
  #   foreach(i=1:cv_times_EN, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
  #     ## split data for cv
  #     dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
  #     
  #     ## training data
  #     ElFitMse <- glmnet_model(train_input = as.matrix(dat$trainX), 
  #                               train_output = as.data.frame(dat$trainY),
  #                               val_input = as.matrix(dat$validX), 
  #                               val_output = as.data.frame(dat$validY),
  #                               lam = lamEN, al = alEN[alpha_])
  #     ElFitMse
  #   }
  # stopCluster(cl)
  # close(pb)
  # # find the optimal tunnning parameters
  # ENExpectedMSE <-apply(ENLoop, 1, mean)
  # optimalEN <- tunParamEN[which.min(ENExpectedMSE),]
}



