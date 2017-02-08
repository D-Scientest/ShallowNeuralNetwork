# Data manipulation
library(magrittr)
library(data.table)
library(reshape2)
library(dplyr)

# Visulization
library(ggplot2)
library(gridExtra)

# Model
library(depmixS4) # HMM
library(kernlab) # Kernnel regression
library(glmnet) # Ridge regression
library(earth) # Piecewise Spline
library(mgcv) # Smooth Spline


# Parallel computation
library(doSNOW) 
library(foreach)

# Private function
source('source/splitBlocksCV.R')
