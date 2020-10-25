simExample = function(){
  set.seed(123)
  # default settings 
  modelName = "QL2"
  smallReward = 0 
  iti = 2
  
  # random seed
  set.seed(123)
  
  # create output directories
  dir.create("figures")
  dir.create("figures/simulation/")
  dir.create("figures/simulation/example")
  
  # load experiment parameters
  load('expParas.RData')
  
  # normative analysis 
  normResults = expSchematics(smallReward, iti, F)
  optimRewardRates = normResults$optimRewardRates
  optimWaitThresholds = normResults$optimWaitThresholds
  
  # load packages and sub functions 
  library("tidyverse")
  source("subFxs/plotThemes.R")
  source("subFxs/helpFxs.R") 
  source("subFxs/loadFxs.R") # 
  source("subFxs/analysisFxs.R") 
  
  
  # get the generative model 
  source(sprintf("subFxs/simModels/default/%s.R", modelName))
  simModel = get(modelName)
  paraNames = getParaNames(modelName)
  paraNames = factor(paraNames, levels = paraNames)
  nPara = length(paraNames)

  # num of repetitions 
  nRep = 10
  duration = 120 * 60
  periodBreaks = seq(0, 16 * 60, length.out = 5)
  
  HPSim_ = list()
  LPSim_ = list()
  HPauc_ = matrix(NA, nrow =  5, ncol = nRep)
  LPauc_ = matrix(NA, nrow =  5, ncol = nRep)
  for(i in 1 : nRep){
    # HP 
    tempt = simModel(c(0.05, 0.05, 5, 0.85, 6), "HP", duration, normResults)
    # tempt = simModel(c(0.1, 0.1, 2, 0.85, 3), "HP", duration, normResults)
    HPSim_[[i]] = tempt
    tempt$Qwaits_ = NULL
    tempt$Gs_ = NULL
    tempt = as.data.frame(tempt)
    HPauc_[1 : 4,i] = sapply(1 : 4, function(x) kmsc(tempt[(i-1) * 4 * 60 <= tempt$sellTime & tempt$sellTime < i * 4 * 60,], min(delayMaxs), F, kmGrid)$auc)
    HPauc_[5,i] = kmsc(tempt[116 * 60 <= tempt$sellTime,], min(delayMaxs), F, kmGrid)$auc
    
    # LP
    tempt  = simModel(c(0.05, 0.05, 5, 0.85, 6), "LP", duration, normResults)
    # tempt  = simModel(c(0.05, 0.05, 2, 0.85, 3), "LP", duration, normResults)
    LPSim_[[i]] = tempt
    tempt$Qwaits_ = NULL;
    tempt$Gs_ = NULL
    tempt = as.data.frame(tempt)
    LPauc_[1 : 4,i] = sapply(1 : 4, function(x) kmsc(tempt[(i-1) * 4 * 60 <= tempt$sellTime & tempt$sellTime < i * 4 * 60,], min(delayMaxs), F, kmGrid)$auc)
    LPauc_[5,i] = kmsc(tempt[116 * 60 <= tempt$sellTime,], min(delayMaxs), F, kmGrid)$auc
  }
  
  # plot LP
  
  
  # asymptotic AUCs
  asyHPAUC = mean(HPauc_[5,])
  asyLPAUC = mean(LPauc_[5,])
  
  # plot AUCs in HP and LP
  data.frame(
    auc = c(as.vector(HPauc_[1:4,]), as.vector(LPauc_[1:4,])),
    condition = c(rep("HP", nRep * 4), rep("LP", nRep * 4)),
    t= rep(rep(seq(0,12, by = 4) + 2,  each = nRep), 2)
  ) %>% group_by(condition, t) %>%
    summarise(mu = mean(auc),
              se = sd(auc) / sqrt(length(auc)),
              min = mu - se,
              max = mu + se) %>% 
    ggplot(aes(t, mu, color = condition)) + geom_point() + geom_line() +
    myTheme + scale_color_manual(values = conditionColors) +
    scale_x_continuous(breaks = seq(0, 16, by = 4),
                       limits = c(0,16)) +
    xlab("Time (s)") +
    ylab("AUC (s)") + theme(legend.position = "none")  +
    ylim(c(0, 20)) + 
    geom_hline(yintercept = 2.2, color = conditionColors[2], linetype = "dashed") +
    geom_hline(yintercept = 20, color = conditionColors[1], linetype = "dashed") +
    geom_point(x = 16, y = asyHPAUC, color = conditionColors[1], shape = 8) + 
    geom_point(x = 16, y = asyLPAUC, color = conditionColors[2], shape = 8)
    ggsave("figures/simulation/example/auc.png", width = 3, height = 3)
  
  # plot 
  exampleSim = HPSim_[[1]]
  exampleQwaits = exampleSim$Qwaits_
  exampleGs = exampleSim$Gs_
  exampleRs = exampleSim$trialEarnings
  exampleTs = exampleSim$timeWaited
  exampleTs[exampleRs == 10] = exampleSim$scheduledWait[exampleRs== 10]
  exampleV0 = exampleSim$V0_
  
  a = lapply(LPSim_, FUN = function(i) sum(LPSim_$trialEarnings))
  ## plot Qwaits
  for(i in 1 : 4){
    if(i > 1){
      rewardColor = ifelse(exampleRs[i-1] == 10, "red", "black")
      data.frame(
        time = 0 : (nrow(exampleQwaits) + 1),
        Qwait = c(exampleV0[i], NA, as.vector(exampleQwaits[,i]))
      ) %>% ggplot(aes(time, Qwait)) + geom_point(color = conditionColors[1]) +
        myTheme + ylab("Value") + xlab("t") +
        ylim(c(0, 12)) +
        geom_point(aes(exampleTs[i-1] + 2, 12), color =  rewardColor, inherit.aes = F, size = 4) +
        ylim(c(0, 12)) 
    }else{
      data.frame(
        time = 0 : (nrow(exampleQwaits) + 1),
        Qwait = c(exampleV0[i], NA, as.vector(exampleQwaits[,i]))
      ) %>% ggplot(aes(time, Qwait)) + geom_point(color = conditionColors[1]) +
        myTheme + ylab("Value") + xlab("t") +
        ylim(c(0, 12)) 
    }
    ggsave(sprintf("figures/simulation/example/snippet-Qwait%d.png", i), width = 3, height = 3) 
  }

  ## plot Gs
  for(i in 1 : 4){
    G = exampleSim$Gs_[,i]
    G[G == 0] = NA
    data.frame(
      time = 0 : (nrow(exampleQwaits) + 1),
      G = c(G[1] * 0.85, NA,  G)
    ) %>% ggplot(aes(time, G)) + geom_point(color = conditionColors[1]) +
      myTheme  + ylab("G(t)") + xlab("t") + ylim(0, 15)
    ggsave(sprintf("figures/simulation/example/snippet-G%d.png", i), width = 3, height = 3) 
  }
  
  ## plot action
  tWaits = 1 : 21
  for(i in 1 : 3){
    action = ifelse(tWaits <= exampleSim$timeWaited[i], 1, NA)
    f = data.frame(
      time = 1 : nrow(exampleQwaits),
      action = action
    ) %>% ggplot(aes(time, action)) + geom_point() +
      myTheme  + ylab("Wait") + xlab("t") + 
      ylim(c(0,1))
    if(exampleSim$trialEarnings[i] > 0){
      f = f + geom_point(aes(x = ))
    }
    
  }
  
  # relative value of waiting in HP and LP
  nCut = 5 # five obervations
  nHPSteps = nrow(HPSim_[[1]]$Qwaits_) - 1
  HPRvWaits_ = matrix(NA, nrow = nHPSteps, ncol = nRep * nCut)
  for(i in 1 : nRep){
    junk = sapply(c(240, 480, 720), function(x) which.min(abs(HPSim_[[i]]$sellTime - x)))
    HPRvWaits_[, (i-1) * nCut + 1 : nCut] = HPSim_[[i]]$Qwaits_[1 :nHPSteps ,c(1, junk,length(HPSim_[[i]]$sellTime))] -
      matrix(rep(HPSim_[[i]]$V0_[c(1, junk,length(HPSim_[[i]]$sellTime))], each = nHPSteps), ncol = nCut)
  }
  nLPSteps = nrow(LPSim_[[1]]$Qwaits_) - 1
  LPRvWaits_ = matrix(NA, nrow = nLPSteps, ncol = nRep * nCut)
  for(i in 1 : nRep){
    junk = sapply(c(240, 480, 720), function(x) which.min(abs(LPSim_[[i]]$sellTime - x)))
    LPRvWaits_[, (i-1) * nCut + 1 : nCut] = LPSim_[[i]]$Qwaits_[1 : nLPSteps,c(1, junk,length(LPSim_[[i]]$sellTime))] -
      matrix(rep(LPSim_[[i]]$V0_[c(1, junk,length(LPSim_[[i]]$sellTime))], each = nLPSteps), ncol = nCut)
  }
  HPdata = data.frame(
    rvWait = as.vector(HPRvWaits_),
    id = rep(1 : nRep, each = nCut * nHPSteps),
    period = rep(rep(1 : nCut, nRep), each = nHPSteps),
    t = rep(1:nHPSteps, nRep * nCut),
    color = rep(rep(1 : nCut, nRep), each = nHPSteps),
    condition = "HP"
  ) 
  LPdata =  data.frame(
    rvWait = as.vector(LPRvWaits_),
    id = rep(1 : nRep, each = nCut * nLPSteps),
    period = rep(rep(1 : nCut, nRep), each = nLPSteps),
    t = rep(1:nLPSteps, nRep * nCut),
    color = rep(rep(1 : nCut + nCut, nRep), each = nLPSteps),
    condition = "LP"
  ) 
  # asymptotic Q(wait, t)
  asyRvWaitHP = HPdata %>% filter(period == 5) %>% group_by(t, condition) %>% summarise(mean(rvWait))
  asyRvWaitLP = LPdata %>% filter(period == 5) %>% group_by(t, condition) %>% summarise(mean(rvWait))
  asyDf = rbind(asyRvWaitHP, asyRvWaitLP)
  names(asyDf) = c("t", "condition", "rvWait")
  # plot Qwaits 
  plotData = rbind(HPdata, LPdata)
  plotData %>% filter(period < 5) %>% group_by(period, t, condition) %>%
    summarise(mu = mean(rvWait), color = mean(color)) %>%
    ggplot(aes(t, mu, color = as.factor(color))) + geom_point() + 
    geom_line() + myTheme +
    facet_grid(~condition) + xlab("t (s)") + ylab("Relative value of waiting") +
    scale_color_manual(values = c("#c7e9c0", "#41ab5d", "#006d2c", "#00441b",
                                  "#bcbddc", "#807dba", "#6a51a3", "#3f007d")) +
    theme(legend.position = "none") + 
    geom_point(data = asyDf, aes(x = t, y = rvWait), inherit.aes = F) 
  ggsave("figures/simulation/example/Qwait.png", width = 4, height = 3) 
}





