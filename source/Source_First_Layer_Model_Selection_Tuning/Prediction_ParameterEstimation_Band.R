Prediction_ParameterEstimation_Band <- function(freqIdx_, bandIdx_, state, bandLag, model, optimalTuning = NULL){
  ##################
  ## Import data
  ##################
  state_x_path <- paste0("data/HMM_State_Train/", state, "_X_",bandIdx_,".csv")
  state_y_path <- paste0("data/HMM_State_Train/", state, "_Y_",bandIdx_,".csv")
  state_breakPoint_path <- paste0("data/HMM_State_Train/", state, "Period_breakPoint_",bandIdx_,".csv")
  
  state_x_test_path <- paste0("data/HMM_State_Test/", state, "_X_",bandIdx_,".csv")
  # state_y_test_path <- paste0("data/HMM_State_Test/", state, "_Y_",bandIdx_,".csv")
  state_test_breakPoint_path <- paste0("data/HMM_State_Test/", state, "Period_breakPoint_",bandIdx_,".csv")
  
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
  
  #################
  # Create lag matrix for band
  #######
  # Training Data
  # Rarray <- foreach(senIdx = 1:140, .combine = 'rbind') %do% {
  #   # Each sentence with removing first rows and make up last rows
  #   start <- state_breakPoint_[senIdx, ] + 1; end <- state_breakPoint_[senIdx + 1,]
  #   trains_x <- data.frame(state_x[start:end, ])
  #   RarraySent <- matrix(0,nrow = (end-start + 1), ncol = 1)
  #   RarraySent_Band <- trains_x
  #   for(i in 1:bandLag){
  #     Rr <- trains_x[-(1:i), ]
  #     imputed <- mean(as.numeric(RarraySent_Band[nrow(RarraySent_Band)-i + 1,]))
  #     makeUp <- matrix(imputed, nrow = i, ncol = 70)
  #     colnames(makeUp) <- colnames(Rr)
  #     Rm <- rbind(Rr, makeUp)
  #     RarraySent_Band <- cbind(RarraySent_Band, Rm)
  #   }
  #   RarraySent <- cbind(RarraySent, RarraySent_Band)
  #   RarraySent[,-1]
  # }
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
  #     makeUpTest <- matrix(imputedTest, nrow = i, ncol = 70)
  #     colnames(makeUpTest) <- colnames(RrTest)
  #     RmTest <- rbind(RrTest, makeUpTest)
  #     RarraySentTest_Band <- cbind(RarraySentTest_Band, RmTest)
  #   }
  #   RarraySentTest <- cbind(RarraySentTest, RarraySentTest_Band)
  #   RarraySentTest[,-1]
  # }  
  
  ############
  # Fitting the optimal model and make prediction
  if(model == "OLS"){
    ## training data
    datC <- cbind(state_y, state_x)
    colnames(datC)[1] <- "Y"
    olsFit <- lm(Y~.,data = datC)
    olsPred <- predict(olsFit, state_x_test)
    
    return(olsPred)
    
  } else if(model == 'Ridge'){
    library(glmnet)
    glmnet_model <- function(train_input ,train_output, 
                             val_input, val_output = NULL, 
                             lam, al = 0, test = FALSE){
      # train_input <- as.matrix(dat$trainX); train_output <- as.data.frame(dat$trainY)
      # val_input <- as.matrix(dat$validX); val_output<- as.data.frame(dat$validY)
      # lam <- lamRidge;al <- 0
      if(!test){
        fitglm <- glmnet(x=train_input,y= as.matrix(train_output),family="gaussian",standardize = FALSE, lambda=lam,alpha=al)
        ptest_glm <- predict(fitglm, val_input)
        mse_glm <- apply(as.matrix(ptest_glm), 2, MSE, predY = val_output)
        return(mse_glm)
      }else{
        fitglm <- glmnet(x=train_input,y= as.matrix(train_output),family="gaussian",standardize = FALSE, lambda=lam,alpha=al)
        ptest_glm <- predict(fitglm, val_input)
        return(ptest_glm)
      }
    }
    
    ridgePred <- glmnet_model(train_input = as.matrix(state_x), 
                                train_output = as.data.frame(state_y),
                                val_input = as.matrix(state_x_test), 
                                lam = optimalTuning, test = TRUE)
    return(ridgePred)
  }
}



