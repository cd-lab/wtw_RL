load("expParas.RData")
library("ggplot2"); library("Hmisc"); library("coin")
library("dplyr"); library("tidyr")
source("subFxs/plotThemes.R")
source("subFxs/loadFxs.R") # load blockData and expPara
source("subFxs/helpFxs.R") # getparaNames
source("subFxs/analysisFxs.R") # plotCorrelation and getCorrelation
source('MFAnalysis.R')

# model Name
modelName = "QL2"
paraNames = getParaNames(modelName)
nPara = length(paraNames)

# output directories
dir.create("figures/expParaAnalysis")
saveDir = sprintf("figures/expParaAnalysis/%s", modelName)
dir.create(saveDir)

# 
MFResults = MFAnalysis(isTrct = T)
sumStats = MFResults[['sumStats']]
# load expPara
paraNames = getParaNames(modelName)
nPara = length(paraNames)
parentDir = "genData/expModelFit"
dirName = sprintf("%s/%s",parentDir, modelName)
expPara = loadExpPara(paraNames, dirName)
passCheck = checkFit(paraNames, expPara)


# plot hist 
# paraNames = c("LR", "LP", expression(tau), expression(gamma), "P")
# paraNames = c("LR", "LP", expression(tau), "P")
paraNames = paraNames
expPara$condition = sumStats$condition[1 : nrow(expPara)]
expPara %>% filter(passCheck ) %>% select(c(paraNames, "condition")) %>%
  gather(-c("condition"), key = "para", value = "value") %>%
  mutate(para = factor(para, levels = paraNames, labels = paraNames ))%>%
  ggplot(aes(value)) + geom_histogram(bins = 8) +
  facet_grid(condition ~ para, scales = "free", labeller = label_parsed) + 
  myTheme + xlab(" ") + ylab(" ")
fileName = sprintf("%s/%s/hist.pdf", "figures/expParaAnalysis", modelName)
ggsave(fileName, width = 8, height = 4)

# summary stats for expPara
expPara %>% filter(passCheck) %>% select(c(paraNames)) %>%
  gather(key = "para", value = "value") %>%
  mutate(para = factor(para, levels = paraNames, labels = paraNames ))%>% 
  group_by(para) %>% summarise(mu = mean(value), median = median(value))


# plot the correlation between adaptation and parameters 
adapation = sumStats$muWTW[seq(2, 84, by = 2)] -  sumStats$muWTW[seq(1, 84, by = 2)]
data.frame(adapation, LR = expPara$phi_pos, passCheck) %>%
  filter(passCheck)  %>%
  ggplot(aes(LR, adapation)) + geom_point() +
  xlab("Reward sensitivity") + ylab("Flexibility") + myTheme 

data.frame(adapation, LP = expPara$phi_neg, passCheck) %>%
  filter(passCheck)  %>%
  ggplot(aes(LP, adapation)) + geom_point() +
  xlab("Reward sensitivity") + ylab("Flexibility") + myTheme 





