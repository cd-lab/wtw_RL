# this script plots delay distributions and reward rates in two environments
expSchematics = function(smallReward, iti, isPlot){
library("latex2exp")
# small reward 
smallReward = 0 # get 0 when the agent quits

# load experiment parameters 
load("expParas.RData")
seqHP = rewardDelays$HP
seqLP = rewardDelays$LP

## for display purposes, all variables on continous time
## are discretized into 0.1 secondition time bins
bin = 0.1 # width of a time bin
time = list(
  HP = seq(bin, delayMaxs[1], by = bin),
  LP = seq(bin, delayMaxs[2], by = bin)
) 

## delays in both HP and LP are drawn repeatedly from 8 possible values,
## In HP, they are uniformly distributed from 0 to 16, 
## in LP, they are log-spaced from 0 to 32, therefore forming a heavy tail distribution

# delay PDFs
HP = rep(0, length(time$HP))
LP = rep(0, length(time$LP))
HP[sapply(1 :8, function(i) which.min(abs(time$HP - seqHP[i])))] = 1 / 8
LP[sapply(1 :8, function(i) which.min(abs(time$LP - seqLP[i])))] = 1 / 8
rewardDelayPDFs =list('HP'= HP, 'LP' = LP)

# delay CDFs
rewardDelayCDFs = list('HP' = cumsum(rewardDelayPDFs$HP),
                      'LP' = cumsum(rewardDelayPDFs$LP))

# average waiting durations given different policies
## Here we assume rewards occur at the middle of each time bin
HP = cumsum((time[['HP']]) * rewardDelayPDFs$HP) / cumsum(rewardDelayPDFs$HP)
LP = cumsum((time[['LP']]) * rewardDelayPDFs$LP) / cumsum(rewardDelayPDFs$LP)
meanRewardDelays = list('HP' = HP, 'LP' = LP)

# rewardRates given different policies
HP = tokenValue * rewardDelayCDFs$HP /
  ((meanRewardDelays$HP * rewardDelayCDFs$HP + time[['HP']] * (1 - rewardDelayCDFs$HP)) + iti)
HP[is.nan(HP)] = 0
LP = tokenValue * rewardDelayCDFs$LP /
  ((meanRewardDelays$LP * rewardDelayCDFs$LP + time[['LP']] * (1 - rewardDelayCDFs$LP)) + iti)
rewardRates = list('HP' = HP, 'LP' = LP)


# optimal raward rates and optimal policies
optimWaitThresholds = list()
optimWaitThresholds$HP = time$HP[which.max(HP)]
optimWaitThresholds$LP = time$LP[which.max(LP)]
optimRewardRates = list()
optimRewardRates$HP = max(HP, na.rm = T)
optimRewardRates$LP = max(LP, na.rm = T)

# calculate theoretical value of waiting as a function of elapsed time 
subjectValues = list()
for(cIdx in 1 : 2){
  condition = conditions[cIdx]
  delayMax = delayMaxs[cIdx]
  pdf = rewardDelayPDFs[[cIdx]]
  cdf = rewardDelayCDFs[[cIdx]]
  thisTime = time[[cIdx]]
  Rstar = optimRewardRates[[cIdx]] # opportunity cost
  ts = seq(0, delayMax, by = 0.1) # elapsed time
  
  # initialize 
  thisSubjectValues = vector(length = length(ts)) # waiting value given the elapsed time value
  Tstars = vector(length = length(ts))  # optimal waiting policy given the elapsed time value
  # loop over different elapsed time values
  for(i in 1 : length(ts)){
    t = ts[i] 
    trctTime = thisTime[thisTime > t]
    # given the elapsed time value, t, loop over different waiting policies to find Tstar 
    if(t == delayMax){
      Tstar = t
      gt_max = tokenValue 
    }else{
      Tstar = t
      gt_max = -100
      for(T in seq(t, delayMax, by = 0.1)){
        trctPDF = pdf[thisTime > t] / sum(pdf[thisTime > t])
        at = tokenValue * sum(trctPDF[trctTime <= T])
        trctPDF[trctTime == T] = trctPDF[trctTime == T]
        bt = sum((trctTime[trctTime <= T] - 0.5 * bin - t) * trctPDF[trctTime <= T]) + 
          + (T - t) * sum(trctPDF[trctTime > T]) 
        gt = at - bt * Rstar
        if(gt > gt_max){
          gt_max = gt
          Tstar = T
        }
      }
    }
    thisSubjectValues[i] = gt_max 
    Tstars[i] = Tstar
  }
  subjectValues[[condition]] = thisSubjectValues
}

# plot 
if(isPlot){
  # plot CDFs 
  library('ggplot2')
  source('subFxs/plotThemes.R')
  library("tidyr"); library('dplyr')
  dir.create('figures/expSchematics')
  ## here we extend the HP CDF to 32s for display purposes
  data.frame(CDF = c(0,c(rewardDelayCDFs$HP, rep(1, length(time$LP) - length(time$HP))), 0, rewardDelayCDFs$LP),
             time = c(0, time$LP, 0, time$LP),
             condition =  rep(c('HP', 'LP'), c(length(time$LP) + 1, length(time$LP) + 1))) %>%
    ggplot(aes(time, CDF)) + geom_line(size = 3, aes(color = condition)) + facet_grid(~condition) +
    ylim(c(0,1)) + scale_y_continuous(breaks = c(0,0.5,1)) + 
    scale_x_continuous(breaks = c(0, max(delayMaxs)/ 2, max(delayMaxs)),limits = c(0, max(delayMaxs) * 1.1)) +
    myTheme + xlab('Delay duration (s)') + ylab('CDF')  +
    annotate("text", x = 16, y = 0.4, label =  "+10¢", size = 6) + 
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    scale_color_manual(values = conditionColors) 
  ggsave('figures/expSchematics/CDF.eps', width =4, height = 3)
  ggsave('figures/expSchematics/CDF.png', width =4, height = 3)
  
  # plot reward rates
  optimData = data.frame(condition = c("HP", "LP"), waitThreshold = as.double(optimWaitThresholds))
  
  data.frame(rewardRate = c(NA, rewardRates[[1]], NA, rewardRates[[2]]),
             time = c(0, time[[1]] + iti,  0, time[[2]] + iti),
             condition = rep(c("HP", "LP"), c(length(time$HP) + 1, length(time$LP) + 1))) %>%
    ggplot(aes(time, rewardRate)) +
    geom_line(size = 2, aes(color = condition))  + myTheme + 
    scale_x_continuous(breaks = c(0, 2, 36),limits = c(0, max(delayMaxs) * 1.2),
                       labels = c("0", "", "36")) +
    ylab(TeX("Reward rate $\\rho_T$ (¢ $s^{-1}$)")) + xlab("Give-up time (s)")  +
    scale_y_continuous(breaks = c(0, 0.4, 0.8, 1.2), limits = c(0, 1.3)) +
    theme(plot.title = element_text(hjust = 0.5, color = themeColor)) +
    scale_color_manual(values = conditionColors) +
    theme(legend.position = "none") + facet_grid(~condition)
  ggsave("figures/expSchematics/reward_rate.eps", width = 4, height = 3)
  ggsave("figures/expSchematics/reward_rate.png", width = 4, height = 3)
  
  # plot subjective value of waiting 
  data.frame(
    value =  c(subjectValues$HP, subjectValues$LP),
    t = c(seq(0, delayMaxs[1], by = 0.1), seq(0, delayMaxs[2], by = 0.1)),
    condition = rep(conditions, c(length(subjectValues$HP), length(subjectValues$LP))))%>%
    ggplot(aes(t, value)) +
    geom_line(aes(color = condition), size = 2) +
    myTheme+
    scale_color_manual(values = conditionColors) +
    scale_linetype_manual(values = c(1, 2)) +
    xlab("Elapsed time (s)") + ylab("Subjective value (¢)")  + 
    theme(legend.position = "none") + facet_grid(~condition)
  ggsave("figures/expSchematics/subjective.eps", width = 4, height = 3)
  ggsave("figures/expSchematics/subjective.png", width = 4, height = 3)       
}

# return outputs 
outputs = list(
  "optimWaitThresholds" = optimWaitThresholds,
  "optimRewardRates" = optimRewardRates,
  "subjectValues" = subjectValues
)
return(outputs)
}
