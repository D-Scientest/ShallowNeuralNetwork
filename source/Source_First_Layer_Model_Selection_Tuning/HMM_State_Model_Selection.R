HMM_State_Model_Selection <- function(freqIdx_, bandIdx_, state, bandLag, queryNum){
  ##################
  ## Import data
  ##################
  state_x_path <- paste0("data/HMM_State_Train/", state, "_X_",bandIdx_,".csv")
  state_y_path <- paste0("data/HMM_State_Train/", state, "_Y_",bandIdx_,".csv")
  state_breakPoint_path <- paste0("data/HMM_State_Train/", state, "Period_breakPoint_",bandIdx_,".csv")
  
  state_x <- as.data.frame(fread(state_x_path))
  state_y <- as.data.frame(fread(state_y_path))[,freqIdx_]
  state_breakPoint <- as.data.frame(fread(state_breakPoint_path))
  state_breakPoint_ <- rbind(0,state_breakPoint)
  
  # Helper function
  MSE = function(Y, predY) {sqrt(mean((Y - predY)^2))} ## Compute MSE
  
  
  #################
  # Create lag matrix for band
  Rarray <- foreach(senIdx = 1:140, .combine = 'rbind') %do% {
    # Each sentence with removing first rows and make up last rows
    start <- state_breakPoint_[senIdx, ] + 1; end <- state_breakPoint_[senIdx + 1,]
    trains_x <- data.frame(state_x[start:end, ])
    RarraySent <- matrix(0,nrow = (end-start + 1), ncol = 1)
    RarraySent_Band <- trains_x
    for(i in 1:bandLag){
      Rr <- trains_x[-(1:i), ]
      imputed <- mean(as.numeric(RarraySent_Band[nrow(RarraySent_Band)-i + 1,]))
      makeUp <- matrix(imputed, nrow = i, ncol = 70)
      colnames(makeUp) <- colnames(Rr)
      Rm <- rbind(Rr, makeUp)
      RarraySent_Band <- cbind(RarraySent_Band, Rm)
    }
    RarraySent <- cbind(RarraySent, RarraySent_Band)
    RarraySent[,-1]
  }
  # state_x_lag <-  paste0("data/HMM_State_Lag/", state, "_X_",bandIdx_,"_lag_",bandLag,".csv")
  # write.table(Rarray, state_x_lag, col.names = FALSE, row.names = FALSE, sep = ',')
  # Rarray <- read.csv(state_x_lag, header = F)
  
  ################################
  #
  # Fitting models
  #
  ################################
  ##########
  # Data splition
  ##########
  #########
  # Query data
  # note: Query data is the last 25 sentences
  start_query <- state_breakPoint_[queryNum,] + 1;end_query <- state_breakPoint_[141,]
  query_x <- Rarray[start_query:end_query, ]
  query_y <- state_y[start_query:end_query]
  #########
  # Trainig and validation data
  TV_x <- Rarray[1:start_query, ]
  TV_y <- as.data.frame(state_y[1:start_query])
  TV_breakPoint <- state_breakPoint_[2:(queryNum + 1), ]
  TV_breakPoint_ <- state_breakPoint_[1:(queryNum + 1), ]

  ################
  # OLS
  ## initialize parameters for k-fold parameter tuning
  cv_times_ols = 100; 
  
  cl <- parallel:::detectCores(); makeCluster(cl)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = cv_times_ols, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  source('splitBlocksCV.R', local = TRUE)
  ## ridge in a parallel loop
  olsLoop <- foreach(i=1:cv_times_ols, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
    ## split data for cv
    dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
    
    ## training data
    datC <- cbind(dat$trainY,dat$trainX)
    colnames(datC)[1] <- "Y"
    olsFit <- lm(Y~.,data = datC)
    olsFitMSE <- MSE(Y = dat$validY,predY = predict(olsFit, dat$validX))
  }
  stopCluster(cl)
  close(pb)
  ridgeExpectedMSE <-mean(olsLoop)

  
  
  ################
  # Piecewise Spline
  library(earth)
  degreeSpline <- c(1,2); cv_times_spline = 10;
  tuningParam <- expand.grid(1:cv_times_spline, degreeSpline)
  
  cl <- parallel:::detectCores(); cl <- makeCluster(cl)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = nrow(tuningParam), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  source('splitBlocksCV.R', local = TRUE)
  ## ridge in a parallel loop
  splineLoop <- foreach(i=1:nrow(tuningParam), .combine='cbind', .options.snow = opts, .packages = 'earth') %dopar% {
    ## split data for cv
    dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
    
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
  finalResult <- cbind(tuningParam[,2], splineLoop)
  names(finalResult) <- c("tunig","mse")
  splineExpectedMSE <- aggregate(finalResult$mse, by=list(finalResult$tuning), FUN=mean)
  optimalSpline <- degreeSpline[which.min(splineExpectedMSE)]
  
  
  
  #####
  # SVM
  #####
  # library(e1071)
  # 
  # lamRidge = exp(seq(-8,8, length.out = 100));
  # cv_times_ridge = 100;
  # 
  # parallel:::detectCores(); cl <- makeCluster(8)
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = cv_times_ridge, style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # source('splitBlocksCV.R', local = TRUE)
  # ## ridge in a parallel loop
  # ridgeLoop <- foreach(i=1:cv_times_ridge, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
  #   ## split data for cv
  #   dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
  #   
  #   svm.model <- svm(x = dat$trainX, y = dat$trainY, kernel = 'linear', cost = 100)
  #   svm.pred <- predict(svm.model, testset[,-3])
  #   
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
  
  
  
  # ########
  # # Ridge:
  # ########
  # library(glmnet)
  # glmnet_model <- function(train_input ,train_output,
  #                          val_input, val_output,
  #                          lam, al = 0){
  #   # train_input <- as.matrix(dat$trainX); train_output <- as.data.frame(dat$trainY)
  #   # val_input <- as.matrix(dat$validX); val_output<- as.data.frame(dat$validY)
  #   # lam <- lamRidge;al <- 0
  #   fitglm <- glmnet(x=train_input,y= as.matrix(train_output),family="gaussian",standardize = FALSE, lambda=lam,alpha=al)
  #   ptest_glm <- predict(fitglm, val_input)
  #   mse_glm <- apply(as.matrix(ptest_glm), 2, MSE, predY = val_output)
  #   return(mse_glm)
  # }
  # 
  # ## initialize parameters for k-fold parameter tuning
  # lamRidge = exp(seq(-8,8, length.out = 100));
  # cv_times_ridge = 100;
  # 
  # parallel:::detectCores(); cl <- makeCluster(8)
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = cv_times_ridge, style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # source('splitBlocksCV.R', local = TRUE)
  # ## ridge in a parallel loop
  # ridgeLoop <- foreach(i=1:cv_times_ridge, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
  #   ## split data for cv
  #   dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
  # 
  #   ## training data
  #   ridgeFitMse <- glmnet_model(train_input = as.matrix(dat$trainX),
  #                               train_output = as.data.frame(dat$trainY),
  #                               val_input = as.matrix(dat$validX),
  #                               val_output = as.data.frame(dat$validY),
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
  
  
  # ########
  # # Polinomial Kernel
  # ########
  # library(kernlab);
  # ## initialize parameters for k-fold parameter tuning
  # cv_times_poly <- 4
  # TuneParam = expand.grid(lambda = c(0.1, 0.5, 1, 2), c = c(-1, 1),
  #                         d = c(1, 2, 3), seed = seq(1, cv_times_poly, by = 1))
  # parallel:::detectCores(); cl <- makeCluster(8, outfile="")
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = nrow(TuneParam), style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # source('splitBlocksCV.R', local = TRUE)
  # PolyKLoop <- foreach(i=1:nrow(TuneParam), .combine='rbind', .options.snow = opts, .packages = 'kernlab') %dopar% {
  #   ## split data for cv
  #   set.seed(TuneParam$seed[i])
  #   dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
  #   ## train
  #   KtrainX = kernelMatrix(polydot(degree = TuneParam$d[i],
  #                                  offset = TuneParam$c[i]), as.matrix(dat$trainX))
  #   alpha = solve(KtrainX + TuneParam$lambda[i] * diag(nrow(KtrainX))) %*% as.matrix(dat$trainY)
  #   
  #   ## predict validation
  #   KvalidX = kernelMatrix(polydot(degree = TuneParam$d[i],
  #                                  offset = TuneParam$c[i]), as.matrix(dat$validX), as.matrix(dat$trainX))
  #   PvalidY = KvalidX %*% alpha
  #   
  #   MSE(PvalidY, dat$validY)
  # }
  # stopCluster(cl)
  # close(pb)
  # PolytuneResult = cbind(TuneParam, PolyKLoop)
  # Optlambda = PolytuneResult$lambda[which.min(PolytuneResult$lambda)]
  # Optd = PolytuneResult$d[which.min(PolytuneResult$d)]
  # Optc = PolytuneResult$c[which.min(PolytuneResult$c)]
  # PolyOptparam = c(Optlambda, Optd, Optc)
  # 
  # PolyKLoop_path <- paste0("data/output/stateModelComp_freq",freqIdx_,"_lag",bandLag,"_state",state,"_bandIdx", bandIdx_,"_allThePoly.csv")
  # write.table(PolyKLoop, PolyKLoop_path, col.names = FALSE, row.names = FALSE, sep = ',')

  
  ############################
  #
  # Query: Comparing models
  #
  ############################
  ######
  # Ridge Regression
  ######
  ridgeFitMse_query <- glmnet_model(train_input = as.matrix(TV_x), 
                              train_output = as.data.frame(TV_y),
                              val_input = as.matrix(query_x), 
                              val_output = as.data.frame(query_y),
                              lam = optimalRidge)
  
  
  ######
  # OLS
  ######
  datC_query <- cbind(TV_y,as.matrix(TV_x))
  colnames(datC_query)[1] <- "Y"
  olsFit_query <- lm(Y~.,data = datC_query)
  olsFitMSE_query <- MSE(Y = query_y,predY = predict(olsFit_query, query_x))
  
  # ######
  # # Polinomial Kernnel
  # ######
  # ### train
  # KtrainX_query = kernelMatrix(polydot(degree = PolyOptparam[2],
  #                                offset = PolyOptparam[3]), as.matrix(TV_x))
  # alpha_query = solve(KtrainX_query + PolyOptparam[1] * diag(nrow(KtrainX_query))) %*% as.matrix(TV_y)
  # 
  # ## predict validation
  # KvalidX_query = kernelMatrix(polydot(degree = PolyOptparam[2],
  #                                offset = PolyOptparam[3]), as.matrix(query_x), as.matrix(TV_x))
  # PvalidY_query = KvalidX_query %*% alpha_query
  # PolyKMse_query <- MSE(PvalidY_query, query_y)
  
  ############################
  #
  # report
  #
  ############################
  QueryMSECompare <- c(ridgeFitMse_query, olsFitMSE_query, optimalRidge)
  names(QueryMSECompare) <- c("Ridge Regression", "OLS", "RidgeLambda")
  
  return(QueryMSECompare)
}



