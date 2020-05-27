# this is the analysis script for simualtion results

# change the working directory
setwd("wtw_simulation")

# libraries 
source("subFxs/simExample.R")
source("subFxs/simDefault.R")
source("subFxs/simExtended.R")
source("subFxs/simOneOff.R")
source("subFxs/plotThemes.R")
source("expSchematics.R")
library(ggpubr)
library("latex2exp")

# load expParas
load("expParas.RData")

##################### example simulations #################
# simulate with one example parameter combination 
simExample()

##################### default simulations #################
# simulate with different parameter combinations
simDefault()

###################### robustness analysis ##################
# simulate with different itis and non-zero quit values
itis = c(2, 4, 8)
smallReward.mag = 1
smallReward.probs= c(0, 0.4, 0.8)

# normative analysis for different itis and non-zero quit values
rewardRate_ = list(); condition_ = list(); iti_ = list(); prob_ = list(); t_= list()
count = 1
for(iti in itis){
  for(prob in smallReward.probs){
    normResults = expSchematics(prob * smallReward.mag, iti, F)
    rewardRate_[[count]] = c(normResults$rewardRates$HP, normResults$rewardRates$LP)
    condition_[[count]] = rep(conditions, sapply(normResults$rewardRates, length))
    iti_[[count]] = rep(iti, length(condition_[[count]]))
    prob_[[count]] = rep(prob, length(condition_[[count]]))
    t_[[count]] = unlist(normResults$time)
    count = count + 1
  }
}
iti.labs = c("ITI = 2 s", "ITI = 4 s", "ITI = 8 s")
names(iti.labs) = c("2", "4", "8")
data.frame(
  rewardRate = unlist(rewardRate_),
  condition = unlist(condition_),
  iti = unlist(iti_),
  prob = unlist(prob_),
  t = unlist(t_)
) %>%
  ggplot(aes(t, rewardRate, color = as.factor(prob))) + geom_line() + 
  facet_grid(condition ~ iti, labeller = labeller(iti = iti.labs)) +
  myTheme + ylab(expression(bold("Reward rate (¢ s"^"-1"*")"))) + xlab("Waiting policy (s)") +
  scale_colour_manual(values = c("#bdbdbd", "#737373", "black")) +
  labs(color = TeX(c("p_{1¢}"))) + 
  scale_x_continuous(breaks = c(0, 20, 40))
ggsave("figures/simulation/extended/rewardRate.png", width = 7, height = 3)

# simulation 
p_ = list(length = 9)
count = 1
for(iti in itis){
  for(prob in smallReward.probs){
    p_[[count]] = simExtended(smallReward.mag, prob, iti)
    count = count + 1
  }
}
d = ggarrange(p_[[1]] + rremove("legend") + rremove("ylab") + rremove("xlab"), 
              p_[[2]] + rremove("legend") + rremove("ylab") + rremove("xlab"),
              p_[[3]] + rremove("legend") + rremove("ylab") + rremove("xlab"),
              p_[[4]] + rremove("legend")  + rremove("ylab") + rremove("xlab"),
              p_[[5]] + rremove("legend") + rremove("ylab") + rremove("xlab"),
              p_[[6]] + rremove("legend") + rremove("ylab") + rremove("xlab"),
              p_[[7]] + rremove("legend") + rremove("ylab") + rremove("xlab"),
              p_[[8]] + rremove("legend") +  rremove("ylab") + rremove("xlab"),
              p_[[9]] + rremove("legend") + rremove("ylab") + rremove("xlab"),
              ncol= 3, nrow = 3)
ggsave("figures/simulation/extended/paraEffect.png", d, width = 10, height = 10)

########################### opportunity cost irrelevant version ###################
simOneOff()





