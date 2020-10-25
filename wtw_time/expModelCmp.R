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
  modelNames = c("QL1", "QL2", "RL1", "RL2", "BL", "optim_noise")
  nModel = length(modelNames)
  passCheck_ = matrix(NA, nrow = nSub, ncol = nModel)
  for(i in 1 : nModel){
    modelName = modelNames[i]
    paraNames = getParaNames(modelName)
    expPara = loadExpPara(paraNames, sprintf("genData/expModelFit/%s", modelName))
    passCheck_[,i] = checkFit(paraNames, expPara)
  }
  
  # extract leave-one-out results
  waic_ =  matrix(NA, nSub, nModel)
  for(m in 1 : nModel){
    modelName = modelNames[m]
    for(sIdx in 1 : nSub ){
      id = ids[sIdx]
      fileName = sprintf("genData/expModelFit/%s/s%s_waic.RData", modelName, id)
      load(fileName)
      waic_[sIdx, m] = WAIC$waic
    }
  }
  outputTable = cbind(waic_, passCheck_,
                      ifelse(hdrData$condition[hdrData$stress == "no_stress"] == "HP", 1, 2))
  write.csv(outputTable, "genData/waic.csv")
  
  
  
  # calcualted num best fit
  modelColors = c(
    "#a6cee3",
    "#1f78b4",
    "#b2df8a",
    "#33a02c",
    "#fb9a99",
    "#e31a1c"
  )
  allPass = apply(passCheck_, 1, sum) == nModel
  bestFitNums = sapply(1 : nModel, function(i) sum(apply(waic_[allPass,], 1, which.min) == i))
  bestFitDf = data.frame(
    modelName = factor(modelNames, levels = modelNames),
    bestFitNum = as.vector(bestFitNums)
  )
  
  # plot, grouping all RL models together
  plotData = data.frame(
    modelName = c("RL", "BL"),
    bestFitNum = c(sum(bestFitDf$bestFitNum[bestFitDf$modelName %in% c('QL1', 'QL2', 'RL1', 'RL2')]), bestFitDf$bestFitNum[bestFitDf$modelName == 'BL'])
  )
  pattern.type<-c('vdashes', 'blank')
  plotData = plotData %>% arrange(desc(modelName)) %>%
    mutate(prop = bestFitNum / sum(bestFitNum) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop)
  patternpie(group = plotData$modelName , pct = plotData$prop,
             label = c("RL/QL", "BL"), 
             label.size=4, label.color = "black", label.distance= 1.2, pattern.type=pattern.type,
             pattern.color = c("#33a02c", "black"),
             pattern.line.size=c(30, 10), frame.color='black',frame.size=1.5, pixel=12, density=c(10, 10),
             background.color = c("#a6cee3", "#fb9a99")) 
  ggsave("figures/expModelCmp/RL_benchmarks.png",  width = 4, height = 4)
  
  # compare 4 RL models
  data.frame(
    modelName = factor(modelNames, levels = modelNames),
    bestFitNum = as.vector(bestFitNums)
  ) %>% filter(modelName %in% c("QL1", "QL2", "RL1", "RL2")) %>%
    arrange(desc(modelName)) %>%
    mutate(prop = bestFitNum / sum(bestFitNum) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop ) %>%
    ggplot(aes(x = "", y = prop, fill = modelName)) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
    scale_fill_manual(values = modelColors) + xlab("") + ylab("") +
    geom_text(aes(y = ypos, label = modelName), color = "white", size = 4) +
    theme(legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=0, face = "bold"),
          axis.title=element_text(size= 20, face = "bold"),
          plot.title = element_text(hjust = 0.5, size= 20, face = "bold")) 
  ggsave("figures/expModelCmp/4model.png",  width = 4, height = 4)
  
  # compare all models
  data.frame(
    modelName = factor(modelNames, levels = modelNames),
    modelLabel = factor(c("QL1", "QL2", "RL1", "RL2", "BL", "ON"), levels = c("QL1", "QL2", "RL1", "RL2", "BL", "ON")),
    bestFitNum = as.vector(bestFitNums)
  ) %>% arrange(desc(modelName)) %>%
    mutate(prop = bestFitNum / sum(bestFitNum) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop) %>%
    ggplot(aes(x = "", y = prop, fill = modelName)) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = modelLabel), color = "white", size = 4) +
    scale_fill_manual(values = modelColors) + xlab("") + ylab("") +
    theme(legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=0, face = "bold"),
          axis.title=element_text(size= 20, face = "bold")) 
  ggsave("figures/expModelCmp/6model.png",  width = 4, height = 4)
  
  
  # plot WAIC
  waic_[!passCheck_] = NA
  data.frame(
    modelName = c("QL1", "QL2", "RL1", "RL2", "BL", "ON"),
    mu = apply(waic_, 2, mean, na.rm = T),
    se = apply(waic_, 2, sd, na.rm = T) / sqrt(apply(waic_, 2, function(x) sum(!is.na(x))))
  ) %>% mutate(modelName = factor(modelName, levels = c("QL1", "QL2", "RL1", "RL2", "BL", "ON"))) %>%
    ggplot(aes(modelName, mu, fill = modelName)) + geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = mu - se, ymax = mu + se), width = 0.4) +
    myTheme + xlab("") + ylab("WAIC") +
    scale_fill_manual(values = modelColors) + theme(legend.position = "none") +
    ylim(c(0, 750))
  fileName = "figures/expModelCmp/cmp.eps"
  fileName = "figures/expModelCmp/cmp.png"
  ggsave(filename = fileName,  width = 3, height = 6)
  
}


