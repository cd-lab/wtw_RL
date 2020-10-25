simDefault = function(){
  # default settings 
  modelName = "QL2"
  smallReward = 0
  iti = 2
  
  # random seed
  set.seed(123)
  
  # create output directories
  dir.create("figures/simulation/")
  dir.create("figures/simulation/default")
  
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
  paraLabels = c("alpha[r]","alpha[u]", "tau", "gamma", "eta")
  nPara = length(paraNames)
  
  # num of repetitions 
  nRep = 10
  duration = 120 * 60
  nPeriod = 4
  periodBreaks = seq(0, 16 * 60, length.out = nPeriod + 1)
  
  ######################### simulate with multiple parameter combinations #####################
  # parameter combinations 
  nCut = 4
  paraSampleHPs = data.frame(
    alphaR = seq(0.01, 0.05, length.out = nCut),
    alphaU = seq(0.01, 0.05, length.out = nCut),
    tau = exp(seq(log(0.5), log(8), length.out = nCut)),
    gamma = seq(0.85, 0.95, length.out = nCut),
    prior = seq(0, 1, length.out = nCut)
  )
  paraSampleLPs = data.frame(
    alphaR = seq(0.01, 0.05, length.out = nCut),
    alphaU = seq(0.01, 0.05, length.out = nCut),
    tau = exp(seq(log(0.5), log(8), length.out = nCut)),
    gamma = seq(0.85, 0.95, length.out = nCut),
    prior = seq(2, 6, length.out = nCut)
  )
  
  
  for(condition in conditions){
    if(condition == 'HP'){
      paraSamples = paraSampleHPs
    }else{
      paraSamples = paraSampleLPs
    }
    paraCombs = apply(paraSamples[3, ], 2, rep, nCut * nPara)
    for(pIdx in 1 : nPara){
      paraCombs[(nCut * (pIdx - 1) + 1) : (nCut * pIdx) ,pIdx] = paraSamples[,pIdx] 
    }
    nComb = nrow(paraCombs)
    
    # initialize outputs
    auc_ = matrix(NA, nrow = nComb, ncol = nRep)
    cip_ = matrix(NA, nrow = nComb, ncol = nRep)
    periodAuc_ = matrix(NA, nrow = nComb * nPeriod, ncol = nRep)
    periodCip_ = matrix(NA, nrow = nComb * nPeriod, ncol = nRep)
    asymAuc_ = matrix(NA, nrow = nComb, ncol = nRep)
    asymCip_ = matrix(NA, nrow = nComb, ncol = nRep)
    # loop
    for(i in 1 : nComb){
      paras = paraCombs[i, ]
      for(j in 1 : nRep){
        tempt = simModel(as.numeric(paras), condition, duration, normResults)
        # kmscResults = kmsc(tempt, min(delayMaxs), F, kmGrid)
        # auc_[i, j] = kmscResults$auc
        # cip_[i, j] = kmscResults$stdWTW
        # calculate asymmetric auc
        tempt$Qwaits_ = NULL
        tempt$Gs_ = NULL
        tempt = as.data.frame(tempt)
        kmscResults = kmsc(tempt[tempt$sellTime >= (120 - 4) * 60,], min(delayMaxs), F, kmGrid)
        asymAuc_[i, j] = kmscResults$auc
        asymCip_[i, j] = kmscResults$stdWTW       
        junk = lapply(1 : nPeriod, function(x) kmsc(tempt[periodBreaks[x] <= tempt$sellTime & tempt$sellTime < periodBreaks[x+1],], min(delayMaxs), F, kmGrid))
        periodAuc_[((i - 1) * nPeriod + 1) : (i * nPeriod), j] = sapply(1 : nPeriod, function(i) junk[[i]]$auc)
        periodCip_[((i - 1) * nPeriod + 1) : (i * nPeriod), j] = sapply(1 : nPeriod, function(i) junk[[i]]$stdWTW)
        }
      sumDf = data.frame(
        auc = apply(asymAuc_, 1, mean),
        cip = apply(asymCip_, 1, mean),
        paraLabel = rep(paraLabels, each = nCut),
        paraRank = factor(rep(1 : nCut, nPara)),
        condition = condition
      )
      df = data.frame(
        auc = apply(periodAuc_, 1, mean),
        cip = apply(periodCip_, 1, mean),
        period = rep(1 : nPeriod, nComb),
        t = rep((periodBreaks[1 : nPeriod] + periodBreaks[2 : (nPeriod+1)] )/ 120, nComb),
        paraName = rep(paraNames, each = nCut * nPeriod),
        paraLabel = rep(paraLabels, each = nCut * nPeriod),
        paraRank = factor(rep(rep(1 : nCut, each = nPeriod), nPara)),
        condition = condition
      ) 
      if(condition == "HP"){
        HPdf = df
        HPSumDf = sumDf
      }else{
        LPdf = df
        LPSumDf = sumDf
      }
    }
  }
  
  # plot AUC and CIP
  cutValues = c(
    "#bdbdbd",
    "#737373",
    "#525252",
    "#252525"
  )
  rbind(HPSumDf, LPSumDf) %>%
    mutate(paraLabel = factor(paraLabel, levels = paraLabels)) %>%
    ggplot(aes(paraRank, auc, color = paraRank)) + geom_point() +
    geom_line(group = 1) + facet_grid(condition~paraLabel, labeller = label_parsed) +
    scale_color_manual(values = cutValues) + myTheme 
    
  #  
  rbind(HPSumDf, LPSumDf) %>%
    mutate(paraLabel = factor(paraLabel, levels = paraLabels)) %>%
    ggplot(aes(paraRank, cip, color = paraRank)) + geom_point() +
    geom_line(group = 1) + facet_grid(condition~paraLabel, labeller = label_parsed) +
    scale_color_manual(values = cutValues) + myTheme 
  
  
  
  # plot AUC time courses 

  optimDf = data.frame(
    t = rep(seq(0, 16, by = 4), 10),
    condition = rep(conditions, each = 5 * 5),
    optimThreshold = rep(unlist(optimWaitThresholds), each = 5 * 5),
    paraLabel= rep(rep(paraLabels, each = 5), 2)
  )
  rbind(HPdf, LPdf) %>% 
    mutate(paraLabel = factor(paraLabel, levels = paraLabels)) %>%
    ggplot(aes(t, auc, color = paraRank)) +
    geom_line() + geom_point() +
    facet_grid(condition~paraLabel, labeller = label_parsed) +
    geom_line(data = optimDf, aes(x = t, y = optimThreshold),
              inherit.aes = F, linetype = "dashed", color = rep(conditionColors, each = 25)) +
    scale_color_manual(values = cutValues) +
    myTheme + xlab("Task time (min)") + ylab("AUC (s)") +
    scale_x_continuous(breaks = periodBreaks / 60, limits = c(0, 16 * 60 / 60)) +
    ylim(c(0, 20)) 
  ggsave("figures/simulation/default/paraEffectAUC.png", width = 7, height = 4)
  
  rbind(HPdf, LPdf) %>% 
    mutate(paraLabel = factor(paraLabel, levels = paraLabels)) %>%
    ggplot(aes(t, cip, color = paraRank)) +
    geom_line() + geom_point() +
    facet_grid(condition~paraLabel, labeller = label_parsed)  +
    scale_color_manual(values = cutValues) +
    myTheme + xlab("Task time (min)") + ylab(TeX("CIP (s^2)"))
  ggsave("figures/simulation/default/paraEffectCIP.png", width = 7, height = 4)
}





