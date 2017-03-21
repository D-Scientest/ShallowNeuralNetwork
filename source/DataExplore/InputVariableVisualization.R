##############
##
## Input Variables
##
##############
#####
## Line plot
#####
# Vertical
source("source/EcogExploring_Line.R")
sentenceIdx <- c(1)# sentence index
frequencyIdx <- c(6)# frequency index
freqLength <- 70# electronodes number in the frequency
variableIdx <- 1:5
direction = "vertical"
lengend__ <- FALSE
verticalComp <- EcogExploring_Line(train_x, train_breakPoint, senIdx_ = sentenceIdx, freqIdx_ = frequencyIdx,freqLength_ = freqLength, varIdx_ = variableIdx,direction_ = direction, legend = lengend__)
print(verticalComp + labs(title = "Input variable vertical", x = "frequency", y = "amplitude"))
print('Input vertical data visualization')
browser()

# Horizontal
sentenceIdx <- c(1)
frequencyIdx <- c(1)# necessary to be 1
freqLength <- 420# horizontal data with all the variables
timePoint <- 1:5# first N time point in the sentence
direction = "horizontal"
lengend__ <- FALSE
horizontalComp <- EcogExploring_Line(train_x, train_breakPoint, senIdx_ = sentenceIdx, freqIdx_ = frequencyIdx, freqLength_ = freqLength, timePoint_ = timePoint, direction_ = direction, legend = lengend__)
print(horizontalComp + labs(title = "Input variable horizontal", x = "frequency", y = "amplitude"))
print('Input horizontal data visualization')
browser()


#####
# Value Contour
#####
filled.contour(as.matrix(train_x)[1:334, 1:420], main = "Input value contour") # First sentence
print('Input data value contour')
browser()

#####
# Correlation Contour
#####
x_correlation_p <- as.matrix(cor(as.data.frame(train_x)[,1:420]))# 420 variables correlation
filled.contour(as.matrix(x_correlation_p), main= "Input covariance contour") 
print('Input data correlation contour')