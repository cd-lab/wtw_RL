# replicate behavioral data with individual fitted parameters
expModelRep = function(modelName){
  set.seed(123)
  # create output directories
  dir.create("figures/expModelRep/")
  dir.create(sprintf("figures/expModelRep/%s",modelName))
  dir.create("genData/expModelRep")
  
  # load experiment parameters
  load('expParas.RData')
  
  # load packages and sub functions 
  library("tidyverse")
  source("expSchematics.R")
  source("subFxs/plotThemes.R")
  source("subFxs/helpFxs.R") 
  source("subFxs/loadFxs.R") # 
  source("subFxs/analysisFxs.R") 
  source("subFxs/modelRep.R")
  
  # num of repetitions 
  nRep = 10
  
  # load empirical data 
  allData = loadAllData()
  hdrData = allData$hdrData 
  trialData = allData$trialData       
  ids = hdrData$id
  nSub = length(ids)
  
  # check fit
  paraNames = getParaNames(modelName)
  nPara = length(paraNames)
  expPara = loadExpPara(paraNames, sprintf("genData/expModelFit/%s", modelName))
  passCheck = checkFit(paraNames, expPara)
  
  ################ compare AUCs and CIPs from empirical and replicated data ################
  ## AUCs and CIPs from empirical data 
  source("MFAnalysis.R")
  MFResults = MFAnalysis(isTrct = T)
  
  sumStats = MFResults[['sumStats']]
  muWTWEmp = sumStats$muWTW
  stdWTWEmp = sumStats$stdWTW
  ## WTW from empirical data 
  muWTWRep_ = matrix(NA, nrow = nRep , ncol = nSub)
  stdWTWRep_ = matrix(NA, nrow = nRep, ncol = nSub)
  timeWTW_ =  matrix(NA, nrow = length(tGrid), ncol = nSub)
  
  ## AUCs and CIPs from simulated data 
  repOutputs =  modelRep(trialData, ids, nRep, T, modelName)
  plotData = data.frame(mu =  repOutputs$muWTWRep_mu, std = repOutputs$stdWTWRep_mu,
                        empMu = muWTWEmp, empStd = stdWTWEmp,
                        passCheck = rep(passCheck, each = 2),
                        condition = sumStats$condition) %>% filter(passCheck)
  save(repOutputs, file = sprintf("genData/expModelRep/%s_trct.RData", modelName))
  # 
  # ## plot to compare AUCs
  plotData %>%
    ggplot(aes(empMu, mu)) +
    geom_point(size = 4, aes(color = condition), stroke = 1, shape = 21) +
    geom_abline(slope = 1, intercept = 0)  +
    ylab("Model-generated (s)") + xlab("Observed (s)") + ggtitle(sprintf("AUC, n = %d", sum(passCheck))) +
    myTheme + theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 20), limits = c(-1, 21)) +
    scale_y_continuous(breaks = c(0, 20), limits = c(-1, 21)) +
    scale_color_manual(values = conditionColors) +
    theme(legend.position = "none")
  fileName = sprintf("figures/expModelRep/%s/muWTW_muWTWRep.eps", modelName)
  fileName = sprintf("figures/expModelRep/%s/muWTW_muWTWRep.png", modelName)
  ggsave(filename = fileName,  width = 4, height = 4)

  ## plot to compare CIPs
  plotData %>%
    ggplot(aes(empStd, std, shape = condition)) +
    geom_point(size = 4, aes(color = condition), stroke = 1, shape = 21)  +
    geom_abline(slope = 1, intercept = 0) +
    ylab(expression(bold(paste("Model-generated (s"^2,")")))) +
    xlab(expression(bold(paste("Observed (s"^2,")")))) + ggtitle(sprintf("CIP, n = %d", sum(passCheck))) +
    myTheme + theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    scale_x_continuous(breaks = c(0, 8), limits = c(0, 8.5)) +
    scale_y_continuous(breaks = c(0, 8), limits = c(0, 8.5)) +
    scale_color_manual(values = conditionColors) +
    theme(legend.position = "none")
  fileName = sprintf("figures/expModelRep/%s/stdWTW_stdWTWRep.eps", modelName)
  fileName = sprintf("figures/expModelRep/%s/stdWTW_stdWTWRep.png", modelName)
  ggsave(filename = fileName,  width = 4, height = 4)
  
  ################ compare WTW time courses from empirical and replicated data ################
  repOutputs =  modelRep(trialData, ids, nRep, F, modelName)
  save(repOutputs, file = sprintf("genData/expModelRep/%s.RData", modelName))
  timeWTW_ = repOutputs$timeWTW_
  
  # plot to check time course of learning
  backgroundData = data.frame(
    xmin = c(0, blockSec),
    xmax = c(blockSec, blockSec * 2),
    condition = c("LP", "HP")
  )
  data.frame(
    wtw = as.vector(timeWTW_),
    t = rep(seq(0, blockSec * 2-1, by = 1), nSub),
    condition = rep(sumStats$condition, each = length(tGrid))
  ) %>% group_by(condition, t) %>%
    dplyr::summarise(mu = mean(wtw, na.rm = F),
              se = sd(wtw, na.rm = F) / sqrt(length(wtw)),
              min = mu - se,
              max = mu + se) %>%
    ggplot(aes(t, mu)) + 
    geom_rect(data = backgroundData, aes(xmin = xmin, xmax = xmax, ymin =0, ymax = 16, fill = condition),inherit.aes = F) +
    geom_line() +
    geom_ribbon(aes(ymin=min, ymax=max), alpha = 0.5) +
    scale_color_manual(values = conditionColors) +
    scale_fill_manual(values = conditionColorBacks) + myTheme +
    theme(legend.position = "none") +
    ylab("WTW (s)") + xlab("Task time (min)")  +
    scale_x_continuous(breaks = 0:2 * 10 * 60 , labels = 0:2 * 10 * 60 / 60) + 
    theme(legend.position = "none") + ylim(0, 16) 
  fileName = sprintf("figures/expModelRep/%s/timecourse.eps", modelName)
  ggsave(filename = fileName,  width = 5, height = 3)
  fileName = sprintf("figures/expModelRep/%s/timecourse.png", modelName)
  ggsave(filename = fileName,  width = 5, height = 3)
  
  # plot example participants 
  # model generated 
  repTrialData = repOutputs$repTrialData
  repNo = repOutputs$repNo
  sIdx = 1
  thisRepTrialData = repTrialData[[repNo[1, sIdx]]]
  
  thisRepTrialData = data.frame(thisRepTrialData[1:6])
  trialPlots(thisRepTrialData) +  
    ggtitle("Model-generated") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  ggsave(sprintf("figures/expModelRep/%s/sim1.eps", modelName), width = 6, height = 4)
  ggsave(sprintf("figures/expModelRep/%s/sim1.png", modelName), width = 6, height = 4)
  
  # 
  thisTrialData = trialData[[ids[sIdx]]]
  thisTrialData  = thisTrialData %>% filter(trialStartTime <=  blockSec - max(delayMaxs))
  thisTrialData = block2session(thisTrialData)
  trialPlots(thisTrialData) + ggtitle("Observed") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  ggsave(sprintf("figures/expModelRep/%s/exp1.eps", modelName), width = 6, height = 4)
  ggsave(sprintf("figures/expModelRep/%s/exp1.png", modelName),, width = 6, height = 4)

}

