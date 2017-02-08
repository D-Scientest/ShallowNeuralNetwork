#####################################################
#
# Candidate models for electronodes feature extraction
#     Model list: 
#              Linear: OLS, Ridge,
#              NonLinear: Piecewise Spline, Smoothing Spline,Polynomial kernel
#     Input: freqIdx_: index of compressed y
#            locIdx: electronodes location index
#            state: activated/inhabitated
#            loclag: time lag
#            queryNum: number of observation in query data
#     Return: Query error of each model(25% of the training data)
#     
#####################################################
HMM_State_Model_Selection_Location <- function(freqIdx_, locIdx_, state, locLag, queryNum){
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

  #############
  #
  # Fitting models
  #
  #############
  # Parameters
  # Query data set: take the last 25 sentence as query set
  start_query <- state_breakPoint_[queryNum,] + 1;end_query <- state_breakPoint_[141,]
  query_x <- Rarray[start_query:end_query, ]
  query_y <- state_y[start_query:end_query]
  TV_x <- Rarray[1:start_query, ]
  TV_y <- as.data.frame(state_y[1:start_query])
  TV_breakPoint <- state_breakPoint_[2:(queryNum + 1), ]
  TV_breakPoint_ <- state_breakPoint_[1:(queryNum + 1), ]
  
  ################
  # Piecewise Spline
  degreeSpline <- 1:3; cv_times_spline = 10;
  tuningParam <- expand.grid(1:cv_times_spline, degreeSpline)
  
  cl <- parallel:::detectCores(); cl <- makeCluster((cl))
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = nrow(tuningParam), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  source('source/splitBlocksCV.R', local = TRUE)
  ## ridge in a parallel loop
  splineLoop <- foreach(i=1:nrow(tuningParam), .combine='rbind', .options.snow = opts, .packages = 'earth') %dopar% {
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
  finalResult <- cbind(as.data.frame(tuningParam[,2]), splineLoop)
  names(finalResult) <- c("tuning","mse")
  splineExpectedMSE <- aggregate(finalResult$mse, by=list(finalResult$tuning), FUN=mean)
  optimalSpline <- degreeSpline[which.min(splineExpectedMSE[,2])]
  
  
  ################
  # Smoothing Spline(cubic)
  bs <- '"cr"'
  kSpline <- 1:15
  cv_times_sspline = 10
  tuningParamSpline <- expand.grid(1:cv_times_sspline, kSpline)
  
  cl <- parallel:::detectCores(); cl <- makeCluster((cl -1))
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = nrow(tuningParamSpline), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  source('source/splitBlocksCV.R', local = TRUE)
  ## ridge in a parallel loop
  SsplineLoop <- foreach(i=1:nrow(tuningParamSpline), .combine='rbind', .options.snow = opts,.packages = "mgcv") %dopar% {
    ## split data for cv
    dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
    
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
  finalResult_Sspline <- as.data.frame(cbind(tuningParamSpline[,2], SsplineLoop))
  colnames(finalResult_Sspline) <- c("tuning","mse")
  SsplineExpectedMSE <- aggregate(finalResult_Sspline$mse, by=list(finalResult_Sspline$tuning), FUN=mean)
  optimalSspline <- kSpline[which.min(SsplineExpectedMSE[,2])]
  
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
  # cl <- parallel:::detectCores(); cl <- makeCluster((cl - 1))
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = cv_times_ridge, style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # source('source/splitBlocksCV.R', local = TRUE)
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
  # cl <- parallel:::detectCores(); cl <- makeCluster((cl-1))
  # registerDoSNOW(cl)
  # pb <- txtProgressBar(max = nrow(TuneParam), style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # source('source/splitBlocksCV.R', local = TRUE)
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
  # ######
  # # Ridge Regression
  # ######
  # ridgeFitMse_query <- glmnet_model(train_input = as.matrix(TV_x), 
  #                                   train_output = as.data.frame(TV_y),
  #                                   val_input = as.matrix(query_x), 
  #                                   val_output = as.data.frame(query_y),
  #                                   lam = optimalRidge)
  # 
  
  ######
  # OLS
  ######
  datC_query <- cbind(TV_y,as.matrix(TV_x))
  colnames(datC_query) <- c("Y", paste0("V", 1:6*(locLag+1)))
  olsFit_query <- lm(Y~.,data = datC_query)
  olsFitMSE_query <- MSE(Y = query_y,predY = predict(olsFit_query, query_x))
  
  
  ######
  # Piecewise spline
  fitSpline_query <- earth(Y~., datC_query, degree = optimalSpline)
  predictionsSpline_query <- predict(fitSpline_query, query_x)
  MSESpline_query <- MSE(predictionsSpline_query, query_y)
  
  ######
  # smooth spline
  form <- paste0("Y~s(V1,k=", optimalSspline,",bs =", bs, ")")
  for(idx_ in 2:((locLag+1) * 6)){
    form <- paste0(form, "+s(V", idx_, ",k=",optimalSspline,", bs =", bs, ")")
  }
  form <- as.formula(form)
  
  fitSspline_query <- gam(form,data=datC_query)
  colnames(query_x) <- paste0("V", 1:6*(locLag+1))
  predictionSspline <- predict(fitSspline_query, query_x)
  MSESSpline_query <- MSE(predictionSspline, query_y)
  
  
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
  QueryMSECompare <- c(olsFitMSE_query, MSESpline_query,  optimalSpline,MSESSpline_query, optimalSspline)
  names(QueryMSECompare) <- c("OLS", "PieceSpline","interaction","SmoothSpline", "K")
  
  return(QueryMSECompare)
}



