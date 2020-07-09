load("expParas.RData")
library(latex2exp)
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


# optimism bias
expPara %>% filter(passCheck) %>%
  ggplot(aes(log(alphaR/alphaU))) +
  geom_histogram(bins = 8) +
  myTheme + 
  xlab(TeX('$log(\\alpha_r/\\alpha_u)$')) +
  ylab("Count") +
  geom_vline(aes(xintercept = 0), color = "red", linetype = 2)
ggsave("figures/expParaAnalysis/optimism.eps", width = 6, height = 6)
ggsave("figures/expParaAnalysis/optimism.png", width = 6, height = 6)

# temproal discounting
wilcoxResults = wilcox.test(expPara$gamma - 1)
expPara %>% filter(passCheck) %>% ggplot(aes(gamma)) +
  geom_histogram(bins = 8) +
  myTheme  + 
  xlab(TeX('$\\gamma$')) +
  ylab("Count") + xlim(c(0.65, 1.05))
ggsave("figures/expParaAnalysis/discounting.eps", width = 4, height = 4)
ggsave("figures/expParaAnalysis/discounting.png", width = 4, height = 4)


# summary stats for expPara
expPara %>% filter(passCheck) %>% select(c(paraNames)) %>%
  gather(key = "para", value = "value") %>%
  mutate(para = factor(para, levels = paraNames, labels = paraNames ))%>% 
  group_by(para) %>% summarise(mu = mean(value), median = median(value))

# test 
expPara %>% filter(passCheck) %>% 
  summarise(median(gamma), median(log(alphaR/alphaU)))


wilcoxResults = wilcox.test(expPara$gamma - 1)
logOdds = log(expPara$alphaR / expPara$alphaU)
wilcoxResults = wilcox.test(logOdds)



