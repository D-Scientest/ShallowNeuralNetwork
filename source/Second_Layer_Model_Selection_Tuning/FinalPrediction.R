#########################################################
#
# Second layer prediction based on frequency band/electronodes location feactures 
# extracted from original data
#
#########################################################
compressList <- c(0,1,7,12,17,23,28,31,32)# compressed frequency index 
source("source/Source_Second_Layer_Model_Selection_Tuning/FinalModelCV.R")
train_y <- as.data.frame(fread("data/train_Y_ecog.csv"))
realTrain_y <- as.data.frame(fread("data/train_Y_ecog.csv"))
train_breakPoint <- as.data.frame(fread("data/train_breakpoints.txt"))

Final_Prediction <- matrix(0, nrow = 1554, ncol = 0)
for(freqIdxFull_ in 1:32){
  sortedList <- sort(c(freqIdxFull_, compressList))
  idx <- which(sortedList == freqIdxFull_)
  if(length(idx) == 2) {
    freqIdx_ <- idx[1] - 1
  }else{
    freqIdx_ <- idx -1
  }
  # Trainging data
  NewInputPathBand_Train <-  paste0("data/NewVariable/OLS/Train/Freq",freqIdx_,"_Band.csv")
  NewInputBand_Train <- read.table(NewInputPathBand_Train,sep = ',')
  NewInputPathLocation_Train <-  paste0("data/NewVariable/OLS/Train/Freq",freqIdx_,"_Location.csv")
  NewInputLocation_Train <- read.table(NewInputPathLocation_Train,sep = ',')
  NewInput_Train <- cbind(NewInputBand_Train, NewInputLocation_Train)
  # Testing Data
  NewInputPathBand_Test <-  paste0("data/NewVariable/OLS/Test/Freq",freqIdx_,"_Band.csv")
  NewInputBand_Test <- read.table(NewInputPathBand_Test,sep = ',')
  NewInputPathLocation_Test <-  paste0("data/NewVariable/OLS/Test/Freq",freqIdx_,"_Location.csv")
  NewInputLocation_Test <- read.table(NewInputPathLocation_Test,sep = ',')
  NewInput_Test <- cbind(NewInputBand_Test, NewInputLocation_Test)
  
  freqPred <- FinalModelCV(inputTrain_ = NewInput_Train, outputTrain_ = train_y[,freqIdxFull_], 
                           inputTest_ = NewInput_Test, break_point_ = train_breakPoint, 
                           freqIdx_ = freqIdxFull_, realInputTest_ = as.data.frame(realTrain_y[,freqIdxFull_]))
}

Mse <- NULL
for(freqIdx_ in 1:32){
  mseOutputPath <- paste0("data/Prediction/OLS/freq_",freqIdx_,'.csv')
  mse <- as.numeric(read.table(mseOutputPath, sep = ','))
  Mse <- c(Mse, mse)
}
print(mean(Mse))


