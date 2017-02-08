# setwd('/home/zhenshan/Dropbox/STAT640CompetitionPrivate')
CVOptimalTuning_Band <- function(freqIdx_, bandIdx_, state, bandLag, model){
  # setwd("/home/rstudio/Dropbox/STAT640CompetitionPrivate")
  library(foreach);library(doSNOW);library(data.table)
  
  # freqIdx_ <- 5; bandIdx_ <- 1; state <- 'activated';bandLag <- 2; model = 'Ridge'
  if(model == "OLS"){
    return(0)
  } else if(model == 'Ridge'){
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
    # write.table(Rarray, state_x_lag, col.names = FALSE, row.names = FALSE, sep = ',')
    # Rarray <- read.csv(state_x_lag, header = F)
    
    #############
    #
    # Fitting models
    #
    #############
    # # Query data set: take the last 25 sentence as query set
    # start_query <- state_breakPoint_[queryNum,] + 1;end_query <- state_breakPoint_[141,]
    # query_x <- state_x[start_query:end_query, ]
    # query_y <- state_y[start_query:end_query]
    # TV_x <- state_x[1:start_query, ]
    # TV_y <- as.data.frame(state_y[1:start_query])
    # TV_breakPoint <- state_breakPoint_[2:(queryNum + 1), ]
    # TV_breakPoint_ <- state_breakPoint_[1:(queryNum + 1), ]
    
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
    lamRidge = exp(seq(28,30, length.out = 10000));
    cv_times_ridge = 100; 
    
    core = parallel:::detectCores(); cl <- makeCluster((core - 1))
    registerDoSNOW(cl)
    pb <- txtProgressBar(max = cv_times_ridge, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    source('splitBlocksCV.R', local = TRUE)
    ## ridge in a parallel loop
    ridgeLoop <- foreach(i=1:cv_times_ridge, .combine='cbind', .options.snow = opts, .packages = 'glmnet') %dopar% {
      ## split data for cv
      dat <- splitBlocksCV(X = state_x, Y = as.data.frame(state_y), break_point = state_breakPoint, seed =(i + 100))
      
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
  }
}



