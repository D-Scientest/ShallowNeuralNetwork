# setwd('/home/zhenshan/Dropbox/STAT640CompetitionPrivate')
HMM_State_Model_Selection <- function(freqIdx_, bandIdx_, state, bandLag, queryNum){
  setwd("/home/rstudio/Dropbox/STAT640CompetitionPrivate")
  library(foreach);library(doSNOW)
  ##################
  # Paramemters
  # freqIdx_ <- 5; bandIdx_ <- 1; state <- 'activated';bandLag <- 2; queryNum <- 115
  ##################
  ## Import data
  ##################
  state_x_path <- paste0("data/HMM_State/", state, "_X_",bandIdx_,".csv")
  state_y_path <- paste0("data/HMM_State/", state, "_Y_",bandIdx_,".csv")
  state_breakPoint_path <- paste0("data/HMM_State/", state, "Period_breakPoint_",bandIdx_,".csv")
  
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
  state_x_lag <-  paste0("data/HMM_State_Lag/", state, "_X_",bandIdx_,"_lag_",bandLag,".csv")
  write.table(Rarray, state_x_lag, col.names = FALSE, row.names = FALSE, sep = ',')
  # Rarray <- read.csv(state_x_lag, header = F)
  
  #############
  #
  # Fitting models
  #
  #############
  # Parameters
  # Query data set: take the last 25 sentence as query set
  start_query <- state_breakPoint_[queryNum,] + 1;end_query <- state_breakPoint_[141,]
  query_x <- state_x[start_query:end_query, ]
  query_y <- state_y[start_query:end_query]
  TV_x <- state_x[1:start_query, ]
  TV_y <- as.data.frame(state_y[1:start_query])
  TV_breakPoint <- state_breakPoint_[2:(queryNum + 1), ]
  TV_breakPoint_ <- state_breakPoint_[1:(queryNum + 1), ]
  
  ########
  # Ridge: 
  ########
  library(glmnet)
  glmnet_model <- function(train_input ,train_output, 
                           val_input, val_output, 
                           lam, al = 0){
    # train_input <- as.matrix(dat$trainX); train_output <- as.data.frame(dat$trainY)
    # val_input <- as.matrix(dat$validX); val_output<- as.data.frame(dat$validY)
    # lam <- lamRidge;al <- 0
    fitglm <- glmnet(x=train_input,y= as.matrix(train_output),family="gaussian",standardize = FALSE, lambda=lam,alpha=al)
    ptest_glm <- predict(fitglm, val_input)
    mse_glm <- apply(as.matrix(ptest_glm), 2, MSE, predY = val_output)
    return(mse_glm)
  }
  
  ## initialize parameters for k-fold parameter tuning
  lamRidge = exp(seq(-8,8, length.out = 100));
  cv_times_ridge = 100; 
  
  parallel:::detectCores(); cl <- makeCluster(8)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = cv_times_ridge, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  source('splitBlocksCV.R', local = TRUE)
  ## ridge in a parallel loop
  ridgeLoop <- foreach(i=1:cv_times_ridge, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
    ## split data for cv
    dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
    
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
  # library(Hmisc)
  ridgeExpectedMSE <-apply(ridgeLoop, 1, mean)
  # plot(ridgeExpectedMSE, type = 'l')
  # ridgeSE = sqrt(apply(ridgeLoop,1,var)/cv_times)
  # errbar(1:length(lamRidge),ridgeExpectedMSE,ridgeExpectedMSE+ridgeSE,ridgeExpectedMSE-ridgeSE,add=TRUE)
  optimalRidge <- lamRidge[which.min(ridgeExpectedMSE)]
  
  ########
  # Polinomial Kernel
  ########
  library(kernlab);
  ## initialize parameters for k-fold parameter tuning
  cv_times_poly <- 4
  TuneParam = expand.grid(lambda = c(0.1, 0.5, 1, 2), c = c(-1, 1),
                          d = c(1, 2, 3), seed = seq(1, cv_times_poly, by = 1))
  parallel:::detectCores(); cl <- makeCluster(8, outfile="")
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = nrow(TuneParam), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  source('splitBlocksCV.R', local = TRUE)
  PolyKLoop <- foreach(i=1:nrow(TuneParam), .combine='rbind', .options.snow = opts, .packages = 'kernlab') %dopar% {
    ## split data for cv
    set.seed(TuneParam$seed[i])
    dat <- splitBlocksCV(X = TV_x, Y = TV_y, break_point = list(TV_breakPoint), seed = i)
    ## train
    KtrainX = kernelMatrix(polydot(degree = TuneParam$d[i],
                                   offset = TuneParam$c[i]), as.matrix(dat$trainX))
    alpha = solve(KtrainX + TuneParam$lambda[i] * diag(nrow(KtrainX))) %*% as.matrix(dat$trainY)
    
    ## predict validation
    KvalidX = kernelMatrix(polydot(degree = TuneParam$d[i],
                                   offset = TuneParam$c[i]), as.matrix(dat$validX), as.matrix(dat$trainX))
    PvalidY = KvalidX %*% alpha
    
    MSE(PvalidY, dat$validY)
  }
  stopCluster(cl)
  close(pb)
  PolytuneResult = cbind(TuneParam, PolyKLoop)
  Optlambda = PolytuneResult$lambda[which.min(PolytuneResult$lambda)]
  Optd = PolytuneResult$d[which.min(PolytuneResult$d)]
  Optc = PolytuneResult$c[which.min(PolytuneResult$c)]
  PolyOptparam = c(Optlambda, Optd, Optc)
  
  PolyKLoop_path <- paste0("data/output/stateModelComp_freq",freqIdx_,"_lag",bandLag,"_state",state,"_bandIdx", bandIdx_,"_allThePoly.csv")
  write.table(PolyKLoop, PolyKLoop_path, col.names = FALSE, row.names = FALSE, sep = ',')
  
  
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
  # Polinomial Kernnel
  ######
  ### train
  KtrainX_query = kernelMatrix(polydot(degree = PolyOptparam[2],
                                       offset = PolyOptparam[3]), as.matrix(TV_x))
  alpha_query = solve(KtrainX_query + PolyOptparam[1] * diag(nrow(KtrainX_query))) %*% as.matrix(TV_y)
  
  ## predict validation
  KvalidX_query = kernelMatrix(polydot(degree = PolyOptparam[2],
                                       offset = PolyOptparam[3]), as.matrix(query_x), as.matrix(TV_x))
  PvalidY_query = KvalidX_query %*% alpha_query
  PolyKMse_query <- MSE(PvalidY_query, query_y)
  
  ############################
  #
  # report
  #
  ############################
  QueryMSECompare <- c(ridgeFitMse_query, PolyKMse_query, optimalRidge, PolyOptparam)
  names(QueryMSECompare) <- c("Ridge Regression", "Polynomial Kernnel", "RidgeLambda","PolyLambda", "Degree", "Offset")
  
  return(QueryMSECompare)
}



