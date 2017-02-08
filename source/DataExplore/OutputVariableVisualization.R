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

# Horizontal
sentenceIdx <- c(3) # (3,4,5) multiple sentence for the same time point
frequencyIdx <- 1:32 # all the frequencies
freqLength <- 1
timePoint <- 4:7 # different time point for one sentence
direction = "horizontal"
lengend__ <- TRUE
horizontalComp <- EcogExploring_Line(train_y, train_breakPoint, senIdx_ = sentenceIdx, freqLength_ = freqLength, timePoint_ = timePoint, direction_ = direction, legend = lengend__)
print(horizontalComp + labs(title = "Output variable horizontal", x = "frequency", y = "amplitude"))

#####
# Value Contour
#####
filled.contour(as.matrix(train_y)[1:334, 1:32], main = "Output Value Contour") # First senetence 

y_correlation_p <- as.data.frame(cor(train_y))
varIdx_ <- 1:32
plot <- y_correlation_p %>% 
  select_(.dots = do.call(paste0, list(rep("V", length.out = length(varIdx_)), varIdx_))) %>%
  mutate(id = 1:nrow(.)) %>%
  data.table::melt(., id.vars = "id") %>%
  qplot(x = id, y = value, group = factor(variable), colour = factor(variable), data = ., geom = "line", alpha = 0.2) +
  labs(title = "Output variable correlation", x = "frequency", y = "covariance")
print(plot) # all time point

# Time series correlation(First senetence)
tsTitle = "output first senetence"
pacf(as.vector(train_y$V1[1:334]), lag.max = 100, main = tsTitle)
acf(as.vector(train_y$V1[1:334]), lag.max = 100, main = tsTitle)


