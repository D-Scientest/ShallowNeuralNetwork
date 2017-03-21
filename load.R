####################################################
# Data Description:
#       Input:
#         1. 10 train sentences with 3055 data point(each sentence length: breakpoint_train.txt)
#         2. 5 test sentences with 1554 data point(each sentence length: breakpoint_test.txt)
#         3. all the data points are recorded from 70 electronodes in the cortical surface
#            and each electronode signal is splitted into 6 frequencies by fourior transform
#            420 variables in total
#       Output:
#         1. audio signal is splitted into 32 frequencies by fourior transform; 32 variables in total
#         2. train_Y_phase_ecog + train_Y_ecog is used to reconstruct sound
####################################################

##########
## training 
train_x <- as.data.frame(fread("data/train_X_ecog.csv"))
train_y <- as.data.frame(fread("data/train_Y_ecog.csv"))
train_breakPoint <- as.data.frame(fread("data/train_breakpoints.txt"))
# Read sentence text
file <- "data/train_sentences.txt"
con <- file(description=file, open="r")
sentenceTxt <- readLines(con)
# Read phase data
train_yPhase <- fread('data/train_Y_phase_ecog.csv')

##########
## testing
test_x <- as.data.frame(fread("data/test_X_ecog.csv"))
test_breakPoint <- as.data.frame(fread("data/test_breakpoints.txt"))

##########
## transformed

# Compressing data(increasing the fitting speed)
train_y_compress <- as.data.frame(fread("data/train_Y_ecog_compress.csv"))

# Switch variable order in train_x/test_x (Input data for location_HMM)
train_x_switched <- as.data.frame(fread("data/train_X_ecog_switched.csv"))
test_x_switched <- as.data.frame(fread("data/test_X_ecog_switched.csv"))
