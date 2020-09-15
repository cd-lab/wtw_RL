expModelCmp = function(){
  # libraries and scripts
  library("ggplot2")
  library("dplyr")
  library("tidyr")
  source("subFxs/helpFxs.R")
  source("subFxs/loadFxs.R")
  source("subFxs/plotThemes.R")
  load("expParas.RData")
  dir.create("figures/expModelCmp")
  
  # load data
  allData = loadAllData()
  hdrData = allData$hdrData           
  trialData = allData$trialData       
  ids = hdrData$id             
  nSub = length(ids) 
  
  # check fit
  modelNames = c("QL1", "QL2", "RL1", "RL2", "optim_noise", "optim_noise_bias")
  nModel = length(modelNames)
  passCheck_ = matrix(NA, nrow = nSub, ncol = nModel)
  for(i in 1 : nModel){
    modelName = modelNames[i]
    paraNames = getParaNames(modelName)
    expPara = loadExpPara(paraNames, sprintf("genData/expModelFit/%s", modelName))
    passCheck_[,i] = checkFit(paraNames, expPara)
  }
  
  # extract leave-one-out results
  waic_ = matrix(NA, nSub, nModel)
  for(m in 1 : nModel){
    modelName = modelNames[m]
    for(sIdx in 1 : nSub ){
      id = ids[sIdx]
      fileName = sprintf("genData/expModelFit/%s/s%s_waic.RData", modelName, id)
      load(fileName)
      waic_[sIdx, m] = WAIC$waic
    }
  }
  
  # calculated num best fit
  modelColors = c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c"
  )
  allPass = apply(passCheck_, 1, sum) == nModel
  tempt = apply(waic_[allPass,], 1, which.min)
  bestFitNums = sapply(1 : 6, function(i) sum(tempt == i))
  data.frame(
    modelName = factor(modelNames, levels = modelNames),
    bestFitNum = as.vector(bestFitNums)
  ) %>% ggplot(aes(x = "", y = bestFitNum, fill = modelName)) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
    scale_fill_manual(values = modelColors) + xlab("") + ylab("Participants best described") +
    theme(legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=0, face = "bold"),
          axis.title=element_text(size= 20, face = "bold")) 
  ggsave("figures/expModelCmp/pie.png",  width = 4, height = 4)
  
  
  # 
  outputTable = cbind(waic_, passCheck_)
  write.table(outputTable, "genData/waic.csv", col.names = F, sep = ",")

  # plot WAIC # I think I probably need to change to median here. 
  # BTW the differences are super small
  waic_[!passCheck_] = NA
  waic_[!passCheck_] = NA
  data.frame(
    modelName = c("QL1", "QL2", "RL1", "RL2", "ON", "ONB"),
    mu = apply(waic_, 2, median, na.rm = T),
    se = apply(waic_, 2, sd, na.rm = T) / sqrt(apply(waic_, 2, function(x) sum(!is.na(x))))
  ) %>% mutate(modelName = factor(modelName, levels = c("QL1", "QL2", "RL1", "RL2", "ON", "ONB"))) %>%
    ggplot(aes(modelName, mu, fill = modelName)) + geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mu - se, ymax = mu + se), width = 0.4) +
    myTheme + xlab("") + ylab("WAIC") +
    scale_fill_manual(values = modelColors) + theme(legend.position = "none") +
    ylim(c(0, 750))
  fileName = "figures/expModelCmp/cmp.png"
  ggsave(filename = fileName,  width = 3, height = 6)
}


