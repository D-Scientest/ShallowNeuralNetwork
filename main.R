###########
# Utility
source('package.R')# loading packages
source('load.R')# data input


###########
# Input/Ouput data exploration(visualization)
source('source/DataExplore/InputVariableVisualization.R')
source('source/DataExplore/OutputVariableVisualization.R')


###########
# Create new HMM train/test dataset by splitting it into 
#   two parts: activated & inhabitated
#   two categories: frequency bands & electronode locations
source("source/HMM_State_Splition/HMM_State_output_Band.R")
source("source/HMM_State_Splition/HMM_State_output_Location.R")
source("source/HMM_State_Splition/HMM_State_output_Band_Test.R")
source("source/HMM_State_Splition/HMM_State_output_Location_Test.R")


###########
# First layer forward propogation(parallelized)
#   Goal: 
#        1. model selection for each frequency band(6) and electronodes location(70)
#        2. create new variables(70 + 6 = 76) for second layer
#   Example model: Ordinary Least Square
#training
source('source/First_Layer_Model_Selection_Tuning/Prediction_Band_Train.R')
source('source/First_Layer_Model_Selection_Tuning/Prediction_Location_Train.R')
#testing
source('source/First_Layer_Model_Selection_Tuning/Prediction_Band_Test.R')
source('source/First_Layer_Model_Selection_Tuning/Prediction_Location_Test.R')


###########
# Second layer forward propogation
#   Goal: 1. modeling selection for the new variables(76) from first layer
#         2. make final prediction
#   Example model: Ordinary Least Square
#   Return: plot for each frequency and average MSE
source('source/Second_Layer_Model_Selection_Tuning/FinalPrediction.R')
