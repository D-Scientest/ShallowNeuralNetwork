source('package.R')# loading packages
source('load.R')# data input

# Input/Ouput data visualization
source('source/DataExplore/InputVariableVisualization.R')
source('source/DataExplore/OutputVariableVisualization.R')

# Create New HMM train/test, input/output by neural signal frequency band/electronode location
source("source/HMM_State_Splition/HMM_State_output.R")
source("source/HMM_State_Splition/HMM_State_output_Location.R")
source("source/HMM_State_Splition/HMM_State_output_Location_Test.R")
source("source/HMM_State_Splition/HMM_State_output_Test.R")

# First layer: Modeling Selection for every frequency band(6) and electronodes location(70)
# Take OLS as optimal model for all the bands and frequency as an example

## First layer: Forward propogation
#training
source('source/First_Layer_Model_Selection_Tuning/Prediction_Band_Train.R')
source('source/First_Layer_Model_Selection_Tuning/Prediction_Location_Train.R')
#testing
source('source/First_Layer_Model_Selection_Tuning/Prediction_Band_Test.R')
source('source/First_Layer_Model_Selection_Tuning/Prediction_Location_Test.R')

# Second layer: Modeling Selection and final prediction
source('source/Second_Layer_Model_Selection_Tuning/FinalPrediction.R')
