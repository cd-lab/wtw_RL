# this script is used to demonstrate the effect of 
library('ggplot2')
library('plyr')
library('dplyr')
library('tidyr')
load("expParas.RData")
source("subFxs/helpFxs.R") # getParas
source("subFxs/loadFxs.R") # load scheduledWait from empirical data
source("subFxs/analysisFxs.R") 
source("subFxs/plotThemes.R")

# loop over participants 
library("doMC")
library("foreach")
nCore = parallel::detectCores() -1 # only for the local computer
registerDoMC(nCore)

# load expData
allData = loadAllData()
hdrData = allData$hdrData           
trialData = allData$trialData       
ids = hdrData$ids        
nSub = length(ids)   

# determine modelName and paraNames
modelName = "QL2"
paraNames = getParaNames(modelName)
nPara = length(paraNames)
source(sprintf("subFxs/simModels/%s.R", modelName))

# determine paraSamples
nCut = 5
paraSampleHPs = cbind(
  seq(0.015, 0.065, length.out = nCut),
  seq(0.015, 0.065, length.out = nCut) * 0.5,
  seq(0.5, 3.5, length.out = nCut),
  seq(0.75, 0.90, length.out = nCut),
  c(2, 7, 12, 17, 22)
)

paraSampleLPs = cbind(
  seq(0.015, 0.065, length.out = nCut),
  seq(0.015, 0.065, length.out = nCut) * 0.5,
  seq(0.5, 3.5, length.out = nCut),
  seq(0.75, 0.90, length.out = nCut),
  seq(20, 40, length.out = nCut)
)


# constants for simulation
blockDuration = 7 * 60
tGrid = seq(0, blockDuration, by = 60)
nSim = 10
periodBounds = seq(0, blockDuration, length.out = 6)

# initialize outputs
simFun = get(modelName)
set.seed(231)
aucHP_ = array(NA, dim = c(nCut, nPara))
wtwHP_ = array(NA, dim = c(length(tGrid), nCut, nPara))
aucSDHP_ = array(NA, dim = c(nCut, nPara))
aucLP_ = array(NA, dim = c(nCut, nPara))
wtwLP_ = array(NA, dim = c(length(tGrid), nCut, nPara))
aucSDLP_ = array(NA, dim = c(nCut, nPara))
pdAucHP_ = array(NA, dim = c(5, nCut, nPara))
pdAucSDHP_ = array(NA, dim = c(5, nCut, nPara))
pdAucLP_ = array(NA, dim = c(5, nCut, nPara))
pdAucSDLP_ = array(NA, dim = c(5, nCut, nPara))
# 
condition = "HP"
paraSamples = paraSampleHPs
for(pIdx in 1 : nPara) {
  for(cIdx in 1 : nCut) {
    paras = paraSamples[3,]
    paras[pIdx] = paraSamples[cIdx, pIdx]
    # initialize outputs 
    aucs = vector(length = nSim)
    aucSDs = vector(length = nSim)
    wtws = matrix(NA, nrow = length(tGrid), ncol = nSim)
    pdAucs = matrix(NA, nrow = 5, ncol = nSim)
    pdAucSDs = matrix(NA, nrow = 5, ncol = nSim)
    for(sIdx in  1 : nSim) {
      tempt = simFun(paras, condition, blockDuration)
      kmscResults = kmsc(tempt, min(tMaxs), tGrid)
      aucs[sIdx] = kmscResults$auc
      aucSDs[sIdx] = kmscResults$stdWTW
      wtwReults = wtwTS(tempt, tGrid, min(tMaxs), F)
      wtws[,sIdx] = wtwReults$timeWTW
      pdAucs[,sIdx] = sapply(1 : 5, function(i) kmsc(tempt[periodBounds[i] <= tempt$sellTime & tempt$sellTime < periodBounds[i+1],], min(tMaxs), tGrid)$auc)
      pdAucSDs[,sIdx] = sapply(1 : 5, function(i) kmsc(tempt[periodBounds[i] <= tempt$sellTime & tempt$sellTime < periodBounds[i+1],], min(tMaxs), tGrid)$stdWTW)
    }
    aucHP_[cIdx, pIdx] = mean(aucs)
    aucSDHP_[cIdx, pIdx] = mean(aucSDs)
    wtwHP_[,cIdx, pIdx] = apply(wtws, MARGIN = 1, FUN = function(x) mean(x, na.rm = T))
    pdAucHP_[, cIdx, pIdx] = apply(pdAucs, MARGIN = 1, mean)
    pdAucSDHP_[, cIdx, pIdx]  = apply(pdAucSDs, MARGIN = 1, mean)
  }
}

#
condition = "LP"
paraSamples = paraSampleLPs
for(pIdx in 1 : nPara){
  for(cIdx in 1 : nCut){
    paras = paraSamples[3,]
    paras[pIdx] = paraSamples[cIdx, pIdx]
    # initialize outputs 
    aucs = vector(length = nSim)
    aucSDs = vector(length = nSim)
    wtws = matrix(NA, nrow = length(tGrid), ncol = nSim)
    pdAucs = matrix(NA, nrow = 5, ncol = nSim)
    pdAucSDs = matrix(NA, nrow = 5, ncol = nSim)
    for(sIdx in  1 : nSim) {
      tempt = simFun(paras, condition, blockDuration)
      kmscResults = kmsc(tempt, min(tMaxs), tGrid)
      aucs[sIdx] = kmscResults$auc
      aucSDs[sIdx] = kmscResults$stdWTW
      wtwReults = wtwTS(tempt, tGrid, min(tMaxs), F)
      wtws[,sIdx] = wtwReults$timeWTW
      pdAucs[,sIdx] = sapply(1 : 5, function(i) kmsc(tempt[periodBounds[i] <= tempt$sellTime & tempt$sellTime < periodBounds[i+1],], min(tMaxs), tGrid)$auc)
      pdAucSDs[,sIdx] = sapply(1 : 5, function(i) kmsc(tempt[periodBounds[i] <= tempt$sellTime & tempt$sellTime < periodBounds[i+1],], min(tMaxs), tGrid)$stdWTW)
    }
    aucLP_[cIdx, pIdx] = mean(aucs)
    aucSDLP_[cIdx, pIdx] = mean(aucSDs)
    wtwLP_[,cIdx, pIdx] = apply(wtws, MARGIN = 1, FUN = function(x) mean(x, na.rm = T))
    pdAucLP_[, cIdx, pIdx] = apply(pdAucs, MARGIN = 1, mean)
    pdAucSDLP_[, cIdx, pIdx]  = apply(pdAucSDs, MARGIN = 1, mean)
  }
}

# # plotdata for auc 
# plotData =  data.frame(auc = c(as.vector(aucHP_), as.vector(aucLP_)),
#              aucSD = c(as.vector(aucSDHP_), as.vector(aucSDLP_)),
#              paraValue = c(as.vector(paraSamplesHP), as.vector(paraSamplesLP)),
#              paraName = rep(rep(paraNames, each = nCut),2),
#              condition = rep(conditions, each= nPara * nCut)) %>%
#   mutate(min = auc - aucSD, max = auc + aucSD)
# 
# # plot auc
# ebWidths = c(0.002, 0.001, 0.5, 0.015, 2)
# dir.create("figures/paraEffect")
# for(i in 1 : nPara){
#   paraName = paraNames[i]
#   plotData[plotData$paraName == paraName,]%>%
#     ggplot(aes(paraValue, auc, linetype = condition, shape = condition))  +
#     geom_line(size = 2) +
#     geom_errorbar(aes(ymin = min, ymax = max), width = ebWidths[i], size = 1) +
#     geom_point(size = 8) + myTheme +
#     xlab(paraName) + ylim(c(0, 16))
#   # ggsave(sprintf("figures/paraEffect/%s.eps",  paraName,
#   #                width = 2, height = 2))
# }
# 
# # plotData for wtw
# plotData =
#   data.frame(
#     mean = c(as.vector(wtwLP_), as.vector(wtwLP_)),
#     condition = rep(conditions, each = nPara * length(tGrid) * nCut),
#     time = rep(tGrid, nCut * nPara * 2),
#     paraName = rep(rep(paraNames, each = length(tGrid) * nCut), 2),
#     paraValue = c(rep(as.vector(paraSamplesHP), each = length(tGrid)),
#                   rep(as.vector(paraSamplesLP), each = length(tGrid))))
# 
# 
# # we lost the very first data point here
# for(i in 1 : nPara){
#   paraName = paraNames[i]
#   plotData[plotData$paraName == paraName & plotData$condition == "LP" &
#              plotData$paraValue %in% paraSamples[c(1,3,5),i], ]%>%
#     mutate(paraValue = factor(paraValue, levels = sort(paraSamples[,i]))) %>%
#     ggplot(aes(time, mean, color = paraValue))  +
#     geom_point(size = 2)  + geom_line(linetype = 2) +
#     scale_color_grey() + myTheme
#   ggsave(sprintf("figures/paraEffect/wtw_%s.eps",  paraName,
#                  width = 2, height = 2))
# }

# plot data for pdAUC
plotData =
  data.frame(
    mean = c(as.vector(pdAucHP_), as.vector(pdAucLP_)),
    sd =  c(as.vector(pdAucSDHP_), as.vector(pdAucSDLP_)),
    condition = rep(conditions, each = nPara * 5 * nCut),
    time = rep((periodBounds[1:5] + periodBounds[2:6])/2, nCut * nPara * 2),
    paraName = rep(rep(paraNames, each = 5 * nCut), 2),
    paraValue = c(rep(as.vector(paraSampleHPs), each = 5),
                  rep(as.vector(paraSampleLPs), each = 5)),
    paraRank = c(rep(1:5, each = 5),
                 rep(1:5, each = 5))) %>%
  mutate(max = mean + sd, min = mean - sd)

cutValues = c(
  "#969696",
  "#737373",
  "#252525"
)
for(c in 1 : 2){
  condition = conditions[c]
  if(c == 1){
    paraSamples = paraSampleHPs
  }else{
    paraSamples = paraSampleLPs
  }
  plotData[ plotData$condition == condition &
              plotData$paraRank %in% c(1, 3, 5), ] %>%
    mutate(paraRank = factor(paraRank),
           paraName = factor(paraName, levels = paraNames)) %>%
    ggplot(aes(time, mean, color = paraRank))  +
    geom_point(size = 3)  + geom_line(size = 2) +
    scale_color_manual(values = cutValues) + myTheme +
    geom_hline(yintercept = optimWaitThresholds[[condition]], color = "red", linetype = 2,
               size = 2) + 
    theme(legend.position = "none") + 
    scale_y_continuous(breaks = c(0, min(tMaxs)), limits = c(0, min(tMaxs))) +
    scale_x_continuous(breaks =  c(0, blockDuration), limits = c(0, blockDuration)) +
    facet_grid(~paraName)
    ggsave(sprintf("figures/paraEffect/zAuc_%s.png", condition),
                   width = 10, height = 2)
}

