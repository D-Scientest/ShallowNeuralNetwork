# ShallowNeuralNetwork

## PROJECT DESCRIPTION:
* Goal: Reconstruct human speech from neural signal

* Data Source: Data is from an experiment that clinical doctor electro-nodes in a subject’s cortical surface and subject listens to 2100 human reading sentences. At the same time, recording audio signal and neural signal. 

## IDEA: 
* Basic idea: From neural network: A good neural network model is the one has the modeling structure to extract the important features from data
* Background knowledge: In neural science, different frequency signal is from different depth of brain and the information used to reconstruct audio signal can be linear or non-linear. The same for different electronodes.
* Idea: Two layers modeling
	First Layer: Apply different models, linear or non-linear, to different frequencies and locations and select the optimal one to make the prediction which is the input for the second layer
	Second Layer: Apply different models to the result from the first layer and make the final prediction
* Advantage:
  * Model structure illustrates deep insight from the domain, neural science, knowledge
  * Can be used for distributed computating, becaused of the by step computation

* MODEL IDEA VISUALIZATION 

![alt tag](https://cloud.githubusercontent.com/assets/14370804/22751570/80cd5cac-edfa-11e6-9dc9-36824fd312ae.png)


## METHOD: 
1. Split data into two categories activated or inhabitated(based on neural activity) with Hidden Markov Model
2. First layer: Create new variables by extracting feature from each frequency(6) band and location(70), 
3. Second Layer: Make Final prediction based on 76 new variables from the first layer
	

## HOW TO USE:
Environment(IDE): Rstudio

Steps:
1. open Rstudio
2. click "File" in the menu
3. then "Open Project..."
4. go to README.md directory
5. click open


## CODE STRUCTRE:
* main.R (user interface)
* load.R (load all the data)
* package.R (load all the necessary packages)
* source/DataExpore (data exploration and visualization)
  * HMM_State_Splition (Method step 1)
  * Source_HMM_State_Splition (source file for step 1)
  * First_Layer_Model_Selection_Tuning (Method step 2)
  * Source_First_Layer_Model_Selection_Tuning (source file for step 2)
  * Second_Layer_Model_Selection_Tuning (Method step 3)
  * Source_Second_Layer_Model_Selection_Tuning (source file for step 3)



