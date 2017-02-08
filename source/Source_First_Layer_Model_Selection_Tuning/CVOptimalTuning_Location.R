CVOptimalTuning_Location <- function(freqIdx_, locIdx_, state, locLag, model){
  ##################
  ## Import data
  ##################
  state_x_path <- paste0("data/HMM_State_Location_Train/", state, "_X_",locIdx_,"_Location.csv")
  state_y_path <- paste0("data/HMM_State_Location_Train/", state, "_Y_",locIdx_,"_Location.csv")
  state_breakPoint_path <- paste0("data/HMM_State_Location_Train/", state, "Period_breakPoint_",locIdx_,"_Location.csv")
  
  state_x <- as.data.frame(fread(state_x_path))
  state_y <- as.data.frame(fread(state_y_path))[,freqIdx_]
  state_breakPoint <- as.data.frame(fread(state_breakPoint_path))
  state_breakPoint_ <- rbind(0,state_breakPoint)
  
  # Helper function
  MSE = function(Y, predY) {sqrt(mean((Y - predY)^2))} ## Compute MSE
  
  
  #################
  # Create lag matrix for band
  if(locLag == 0){
    Rarray <- state_x
  }else{
    Rarray <- foreach(senIdx = 1:140, .combine = 'rbind') %do% {
      # Each sentence with removing first rows and make up last rows
      start <- state_breakPoint_[senIdx, ] + 1; end <- state_breakPoint_[senIdx + 1,]
      trains_x <- data.frame(state_x[start:end, ])
      RarraySent <- matrix(0,nrow = (end-start + 1), ncol = 1)
      RarraySent_Loc <- trains_x
      for(i in 1:locLag){
        Rr <- trains_x[-(1:i), ]
        imputed <- mean(as.numeric(RarraySent_Loc[nrow(RarraySent_Loc)-i + 1,]))
        makeUp <- matrix(imputed, nrow = i, ncol = 6)
        colnames(makeUp) <- colnames(Rr)
        Rm <- rbind(Rr, makeUp)
        RarraySent_Loc <- cbind(RarraySent_Loc, Rm)
      }
      RarraySent <- cbind(RarraySent, RarraySent_Loc)
      RarraySent[,-1]
    }
  }
  # state_x_lag <-  paste0("data/HMM_State_Lag/", state, "_X_",bandIdx_,"_lag_",bandLag,".csv")
  # write.table(Rarray, state_x_lag, col.names = FALSE, row.names = FALSE, sep = ',')
  # Rarray <- read.csv(state_x_lag, header = F)
  
  #############
  #
  # Fitting models
  #
  library(glmnet)
  glmnet_model <- function(train_input ,train_output, 
                           val_input, val_output, 
                           lam, al = 0, panelty_factor = NULL){
    # train_input <- as.matrix(dat$trainX); train_output <- as.data.frame(dat$trainY)
    # val_input <- as.matrix(dat$validX); val_output<- as.data.frame(dat$validY)
    # lam <- lamRidge;al <- 0.2 ; panelty_factor <- 1/abs(betals)
    if(is.null(panelty_factor)){
      fitglm <- glmnet(x=train_input,y= as.matrix(train_output),
                       family="gaussian",standardize = FALSE, 
                       lambda=lam,alpha=al)
    }else{
      fitglm <- glmnet(x=train_input,y= as.matrix(train_output),
                       family="gaussian",standardize = FALSE, 
                       lambda=lam,alpha=al, penalty.factor = panelty_factor)
    }
    
    ptest_glm <- predict(fitglm, val_input)
    mse_glm <- apply(as.matrix(ptest_glm), 2, MSE, predY = val_output)
    return(mse_glm)
  }
  
  library(ncvreg)
  ncvreg_model <- function(train_input ,train_output, 
                           val_input, val_output, 
                           lam, panelt){
    # train_input <- as.matrix(dat$trainX); train_output <- dat$trainY
    # val_input <- as.matrix(dat$validX); val_output<- dat$validY
    # lam <- lambdaMCp;panelt <- "MCP"
    fitncv <- ncvreg(train_input,train_output,family="gaussian",lambda=lam, penalty= panelt)
    ptest_ncv <- predict(fitncv, val_input)
    mse_ncv <- apply(as.matrix(ptest_ncv), 2, MSE, predY = val_output)
    return(mse_ncv)
  }
  
  if(model == "Ridge"){
    ## initialize parameters for k-fold parameter tuning
    lamRidge = exp(seq(-8,8, length.out = 1000));
    cv_times_ridge = 100; 
    
    core <-parallel:::detectCores(); cl <- makeCluster((core - 1))
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = cv_times_ridge, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('source/splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    ridgeLoop <- foreach(i=1:cv_times_ridge, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed = (i+1))
      
      ## training data
      ridgeFitMse <- glmnet_model(train_input = as.matrix(dat$trainX), 
                                  train_output = as.data.frame(dat$trainY),
                                  val_input = as.matrix(dat$validX), 
                                  val_output = as.data.frame(dat$validY),
                                  lam = lamRidge)
      ridgeFitMse
    }
    stopCluster(cl)
    close(pb)
    # find the optimal tunnning parameters
    ridgeExpectedMSE <-apply(ridgeLoop, 1, mean)
    optimalRidge <- lamRidge[which.min(ridgeExpectedMSE)]
    
    return(optimalRidge)
  }else if(model == 'Adaptive L'){
    lamAdL = exp(seq(-8,8, length.out = 1000)); cv_times_AdL = 100; 
    
    core <- parallel:::detectCores(); cl <- makeCluster((core-1))
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = cv_times_AdL, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    AdLLoop <- foreach(i=1:cv_times_AdL, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed = (i+1))
      ## training data
      betals = solve(t(as.matrix(dat$trainX))%*%as.matrix(dat$trainX))%*%t(as.matrix(dat$trainX))%*%dat$trainY
      AdLFitMse <- glmnet_model(train_input = as.matrix(dat$trainX), 
                                train_output = as.data.frame(dat$trainY),
                                val_input = as.matrix(dat$validX), 
                                val_output = as.data.frame(dat$validY),
                                lam = lamAdL, al = 1,  1/abs(betals))
      AdLFitMse
    }
    stopCluster(cl)
    close(pb)
    # find the optimal tunnning parameters
    AdlExpectedMSE <-apply(AdLLoop, 1, mean)
    optimalAdl <- lamAdL[which.min(AdlExpectedMSE)]
    
    return(optimalAdl)
  }else if(model == 'MC+'){
    ## initialize parameters for k-fold parameter tuning
    lambdaMCp <- exp(seq(-8,8, length.out = 1000));
    paneltyType <- "MCP"; cv_times_MCp = 100; 
    
    
    core <-parallel:::detectCores(); cl <- makeCluster((core - 1))
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = cv_times_MCp, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    MCpLoop <- foreach(i=1:cv_times_MCp, .combine='cbind', .options.snow = opts, .packages = 'ncvreg') %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed = (i+1))
      
      ## training data
      mcpFitMse <- ncvreg_model(train_input = as.matrix(dat$trainX), 
                                train_output = dat$trainY,
                                val_input = as.matrix(dat$validX), 
                                val_output = dat$validY,
                                lambdaMCp, paneltyType)
      mcpFitMse
    }
    stopCluster(cl)
    close(pb)
    # find the optimal tunnning parameters
    mcpExpectedMSE <- apply(MCpLoop, 1, mean)
    optimalmcp <- lambdaMCp[which.min(mcpExpectedMSE)]
    
    return(optimalmcp)
  }else if(model == 'SCAD'){
    ## initialize parameters for k-fold parameter tuning
    lambdaSCAD <- exp(seq(-8,8, length.out = 1000));
    paneltyType_scad <- "SCAD"; cv_times_SCAD = 100; 
    
    
    core <- parallel:::detectCores(); cl <- makeCluster((core - 1))
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = cv_times_SCAD, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    SCADLoop <- foreach(i=1:cv_times_SCAD, .combine='cbind', .options.snow = opts, .packages = 'ncvreg') %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed = (i+1))
      
      ## training data
      scadFitMse <- ncvreg_model(train_input = as.matrix(dat$trainX), 
                                 train_output = dat$trainY,
                                 val_input = as.matrix(dat$validX), 
                                 val_output = dat$validY,
                                 lambdaSCAD, paneltyType_scad)
      scadFitMse
    }
    stopCluster(cl)
    close(pb)
    # find the optimal tunnning parameters
    scadExpectedMSE <- apply(SCADLoop, 1, mean)
    optimalscad <- lambdaSCAD[which.min(scadExpectedMSE)]
    
    return(optimalscad)
  }else if(model == 'OLS'){
    return(0)
  }else if(model == 'SmoothSpline'){
    library(mgcv)
    bs <- '"cr"';kSpline <- 1:15; cv_times_sspline = 20;
    tuningParamSpline <- expand.grid(1:cv_times_sspline, kSpline)
    
    cl <- parallel:::detectCores(); cl <- makeCluster(3)
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = nrow(tuningParamSpline), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    SsplineLoop <- foreach(i=1:nrow(tuningParamSpline), .combine='rbind', .options.snow = opts,.packages = "mgcv") %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed = (i+1))
      
      datC <- cbind(dat$trainY,dat$trainX)
      colnames(datC) <- c("Y", paste0("V", 1:6*(locLag+1)))
      
      form <- paste0("Y~s(V1,k=", tuningParamSpline[i, 2],",bs =", bs, ")")
      for(idx_ in 2:((locLag+1) * 6)){
        form <- paste0(form, "+s(V", idx_, ",k=",tuningParamSpline[i, 2],", bs =", bs, ")")
      }
      form <- as.formula(form)
      
      fit <- gam(form,data=datC)
      colnames(dat$validX) <- paste0("V", 1:6*(locLag+1))
      predictions <- predict(fit, dat$validX)
      
      MSESSpline <- MSE(predictions, dat$validY)
      
      MSESSpline
    }
    stopCluster(cl)
    close(pb)
    
    # find the optimal tunnning parameters
    # library(Hmisc)
    finalResult_Sspline <- as.data.frame(cbind(tuningParamSpline[,2], SsplineLoop))
    colnames(finalResult_Sspline) <- c("tuning","mse")
    SsplineExpectedMSE <- aggregate(finalResult_Sspline$mse, by=list(finalResult_Sspline$tuning), FUN=mean)
    optimalSspline <- kSpline[which.min(SsplineExpectedMSE[,2])]
    return(optimalSspline)
    
  }else if(model == 'PiecewiseSpline'){
    library(earth)
    degreeSpline <- 1:3; cv_times_spline = 20;
    tuningParam <- expand.grid(1:cv_times_spline, degreeSpline)
    
    cl <- parallel:::detectCores(); cl <- makeCluster(3)
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = nrow(tuningParam), style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    splineLoop <- foreach(i=1:nrow(tuningParam), .combine='rbind', .options.snow = opts, .packages = 'earth') %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed = (i+1))
      
      datC <- cbind(dat$trainY,dat$trainX)
      colnames(datC)[1] <- "Y"
      fit <- earth(Y~., datC, degree = tuningParam[i,2])
      predictions <- predict(fit, dat$validX)
      
      MSESpline <- MSE(predictions, dat$validY)
      
      MSESpline
    }
    stopCluster(cl)
    close(pb)
    
    # find the optimal tunnning parameters
    # library(Hmisc)
    finalResult <- cbind(as.data.frame(tuningParam[,2]), splineLoop)
    names(finalResult) <- c("tuning","mse")
    splineExpectedMSE <- aggregate(finalResult$mse, by=list(finalResult$tuning), FUN=mean)
    optimalSpline <- degreeSpline[which.min(splineExpectedMSE[,2])]
    return(optimalSpline)
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



