####################################################
#
# Data Description:
#       Input:
#         1. 10 train sentences with 3055 data point(sentence length is from breakpoint_train.txt)
#         2. 5 test sentences with 1554 data point(sentence length is from breakpoint_test.txt)
#         3. all the data points are recorded from 70 electronodes in the brain
#            and splitted into 6 frequencies band by fourior transform; 420 variables in total
#       Output:
#         1. all teh data points are splitted into 32 frequencies by fourior transform; 32 variables in total
#         2. train_Y_phase_ecog + train_Y_ecog is used to reconstruct sound
####################################################

## training 
train_x <- as.data.frame(fread("data/train_X_ecog.csv"))
train_y <- as.data.frame(fread("data/train_Y_ecog.csv"))

train_breakPoint <- as.data.frame(fread("data/train_breakpoints.txt"))
# train_breakPoint_ <- rbind(0,train_breakPoint)
# Read sentence text
file <- "data/train_sentences.txt"
con <- file(description=file, open="r")
sentenceTxt <- readLines(con)
# Read phase data
train_yPhase <- fread('data/train_Y_phase_ecog.csv')

## testing
test_x <- fread("data/test_X_ecog.csv")
test_breakPoint <- as.data.frame(fread("data/test_breakpoints.txt"))

## Compressing data(increasing the fitting speed)
train_y_compress <- as.data.frame(fread("data/train_Y_ecog_compress.csv"))

## Input data for location HMM(Variable order swiched)
train_x_switched <- as.data.frame(fread("data/train_X_ecog_switched.csv"))
