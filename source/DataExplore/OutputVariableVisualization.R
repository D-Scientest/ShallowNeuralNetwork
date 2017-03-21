##############
##
## Ouput Variables
##
##############
#####
## Line plot
#####
# Vertical
source("source/EcogExploring_Line.R")
sentenceIdx <- c(1)
frequencyIdx <- 29:31
freqLength <- 1;# frequency band length of output variable
direction = "vertical"
lengend__ <- TRUE
verticalComp <- EcogExploring_Line(train_y, train_breakPoint, senIdx_ = sentenceIdx, freqIdx_ = frequencyIdx ,freqLength_ = freqLength, direction_ = direction, legend = lengend__)
print(verticalComp + labs(title = "Output variable vertical", x = "time point", y = "amplitude"))
print('Output vertical data visualization')
browser()

# Horizontal
sentenceIdx <- c(3) # (3,4,5) multiple sentence for the same time point
frequencyIdx <- 1:32 # all the frequencies
freqLength <- 1
timePoint <- 4:7 # different time point for one sentence
direction = "horizontal"
lengend__ <- TRUE
horizontalComp <- EcogExploring_Line(train_y, train_breakPoint, senIdx_ = sentenceIdx, freqLength_ = freqLength, timePoint_ = timePoint, direction_ = direction, legend = lengend__)
print(horizontalComp + labs(title = "Output variable horizontal", x = "frequency", y = "amplitude"))
print('Output horizontal data visualization')
browser()

#####
# Value Contour
#####
filled.contour(as.matrix(train_y)[1:334, 1:32], main = "Output Value Contour") # First senetence 
print('Output data value contour')
browser()

#####
# Time series correlation(First senetence)
#####
tsTitle = "output first senetence"
pacf(as.vector(train_y$V1[1:334]), lag.max = 100, main = tsTitle)
acf(as.vector(train_y$V1[1:334]), lag.max = 100, main = tsTitle)
print('Output Acf & Pacf')
browser()

