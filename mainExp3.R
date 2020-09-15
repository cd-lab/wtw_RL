# this is the analysis script for Exp3

# change the working directory
setwd("wtw_vary")

# parameters
iti = 2 # inter-trial interval in seconds 
smallReward = 0 # initial token value
modelNames = c("QL1", "QL2", "RL1", "RL2", "optim_noise", "optim_noise_bias")

####################### normative analysis ######################
source("expSchematics.R")
expSchematics(smallReward , iti, isPlot = T)

###################### model-free analysis ########################
source("MFPlot.R")

##################### model fit #############################
## warnings: this function is time-consuming and we don't recommend running it locally
## We saved its outputs for you so you can directly move on to the next step
source("expModelFit.R")
for(modelName in modelNames){
  # fit all participants
  expModelFit(modelName, isFirstFit = F)
  # check whether the model converge and fit again if necessary 
  expModelFit(modelName, isFirstFit = T)
}

################## posterior preditive check ##############
# preditc behavioral observations using individual fitted parameters
source("expModelRep.R")
source("expModelRepAll.R")
for(modelName in modelNames){
  expModelRep(modelName)
}
# plot predictions 
expModelRepAll()


