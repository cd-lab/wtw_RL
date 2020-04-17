library("tidyverse")
library("ggpubr")
library("latex2exp")
source("subFxs/plotThemes.R")
source('expSchematics.R')
source('MFAnalysis.R')

# output dir
dir.create('figures/MFplot')

# load experiment parameters
load("expParas.RData")

# normative analysis
iti = 2
normResults = expSchematics(0, iti, F)
optimWaitThresholds = normResults$optimWaitThresholds
nCondition = length(conditions)

######################### plot WTW timecourses in two environments ######################
MFResults = MFAnalysis(isTrct = F)
sumStats = MFResults[['sumStats']]
timeWTW_ = MFResults[['timeWTW_']]
nSub = nrow(sumStats)
## use background color to distinguish used and excluded data 
colorData = data.frame(
  xmin = c(0, blockSec), xmax = c(blockSec - max(delayMaxs), nBlock * blockSec - max(delayMaxs)),
  condition = conditions
)
greyData = data.frame(
  xmin = 1:2 * blockSec - max(delayMaxs), xmax = 1:2 * blockSec
)
data.frame(wtw = unlist(timeWTW_),
           time = rep(seq(0, blockSec * nBlock -1, by = 1), nSub),
           condition = rep(rep(c("LP", "HP"), each = length(tGrid))), nSub) %>%
  group_by(condition, time) %>%
  dplyr::summarise(mu = mean(wtw, na.rm = F), se = sd(wtw, na.rm = F) / sqrt(sum(!is.na(wtw))),
                   min = mu- se, max = mu + se) %>%
  ggplot(aes(time, mu)) +
  geom_rect(colorData, mapping = aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 20, fill = condition),
            inherit.aes = F) + 
  geom_rect(data = greyData, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 20),
            fill = "#d9d9d9", inherit.aes = F) +
  geom_ribbon(aes(ymin=min, ymax=max), fill = 'grey') +
  geom_line(size = 0.5) +
  xlab("Task time (s)") + ylab("WTW (s)") + 
  scale_y_continuous(breaks = c(0, 10, 20), limits = c(0, 20)) +
  myTheme + scale_x_continuous(breaks = c(0, 600, 1200), labels = c(0, 600, 1200) / 60 ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = themeColor)) +
  scale_fill_manual(values = conditionColorBacks) + 
  theme(legend.position = "none")
ggsave("figures/MFPlot/wtw_timecourse.eps", width = 5, height = 3) 
ggsave("figures/MFPlot/wtw_timecourse.png", width = 5, height = 3) 

################## plot AUCs in two environments ###################
MFResults = MFAnalysis(isTrct = T)
sumStats = MFResults[['sumStats']]
wTest = wilcox.test( sumStats[sumStats$condition == "HP", "muWTW"],
                     sumStats[sumStats$condition == "LP", "muWTW"],paired = T)
data.frame(muWTWHP = sumStats$muWTW[sumStats$condition == 'HP'],
           muWTWLP = sumStats$muWTW[sumStats$condition == 'LP']) %>%
  ggplot(aes(muWTWLP, muWTWHP)) +
  geom_point(size = 5, shape = 21, stroke =1) +
  geom_abline(slope = 1, intercept = 0) + 
  annotate("text", x = 8, y = 3, label = sprintf('p = %0.3f*', wTest$p.value), size = 6) +
  xlab("LP AUC (s)") + ylab("HP AUC (s)") + 
  myTheme + xlim(c(-1,17)) + ylim(c(-1,17)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, color = themeColor)) 
ggsave("figures/MFPlot/muWTW_comparison.eps", width = 4, height = 4)
ggsave("figures/MFPlot/muWTW_comparison.png", width = 4, height = 4)

################### plot CIPs in two environments ###################
# test
wTest = wilcox.test( sumStats[sumStats$condition == "HP", "stdWTW"],
                     sumStats[sumStats$condition == "LP", "stdWTW"],paired = T)
# plot
data.frame(stdWTWHP = sumStats$stdWTW[sumStats$condition == 'HP'],
           stdWTWLP = sumStats$stdWTW[sumStats$condition == 'LP']) %>%
  ggplot(aes(stdWTWLP, stdWTWHP)) +
  geom_point(size = 5, shape = 21, stroke =1) +
  geom_abline(slope = 1, intercept = 0)  +
  xlab(TeX("LP CIP (s^2)")) + ylab(TeX("HP CIP (s^2)")) + 
  myTheme + xlim(c(-1,6)) + ylim(c(-1,6)) 
ggsave("figures/MFPlot/stdWTW_comparison.eps", width = 4, height = 4)
ggsave("figures/MFPlot/stdWTW_comparison.png", width = 4, height = 4)

################### plot CIP and AUC correlations ###################
# test
cor.test(sumStats$muWTW, sumStats$stdWTW, method = "spearman")
cor.test(sumStats$muWTW[sumStats$condition == "HP"], sumStats$stdWTW[sumStats$condition == "HP"], method = "spearman")
cor.test(sumStats$muWTW[sumStats$condition == "LP"], sumStats$stdWTW[sumStats$condition == "LP"], method = "spearman")
# plot
sumStats %>% ggplot(aes(muWTW, stdWTW)) +
  geom_point(aes(color = condition), size = 3) +
  facet_grid(~condition) + myTheme +
  xlab("AUC (s)") + ylab(TeX("CIP ($s^2$)")) +
  scale_color_manual(values = conditionColors) +
  theme(legend.position = "none")
ggsave("figures/MFPlot/stdWTW_muWTW.eps", width = 8, height = 4)
ggsave("figures/MFPlot/stdWTW_muWTW.png", width = 8, height = 4)

################################ plot survival curves #####################
# optimal strategy
optim = data.frame(
  t = rep(kmGrid,  2),
  surv = rep(1, length(kmGrid) * 2),
  condition = rep(conditions, each = length(kmGrid)),
  select = rep(1:2, each = length(kmGrid))
) 
optim$surv[optim$condition == "LP" & kmGrid> optimWaitThresholds$LP] = 0 # quit after 2.2 s
optim$surv[optim$condition == "HP" & kmGrid> optimWaitThresholds$LP] = NA # don't plot after 2.2 s
optim$select[optim$condition == "HP" & kmGrid <= optimWaitThresholds$LP] = rep(1:2, each = 3) # plot interleaving colors 
optim$select[optim$condition == "LP" & kmGrid <= optimWaitThresholds$LP] = rep(1:2, each = 3) # plot interleaving colors
# stats test
survCurve_ = MFResults$survCurve_
plotData = data.frame(survCurve = unlist(survCurve_),
                      time = rep(kmGrid, nSub * nCondition),
                      condition = rep(sumStats$condition, each = length(kmGrid)))
isSig = lapply(1 : length(kmGrid) , function(i)
{
  t = kmGrid[i]
  HP = plotData$survCurve[plotData$condition == "HP" & plotData$time == t]
  LP = plotData$survCurve[plotData$condition == "LP" & plotData$time == t]
  tempt = wilcox.test(HP, LP, paired = T)
  tempt$p.value
}
)
sigData = data.frame(
  isSig = ifelse(isSig < 0.05, 1.01, NA),
  t = kmGrid
)
# plot
plotData %>%
  group_by(condition, time) %>%
  dplyr::(mu = mean(survCurve, na.rm = F), se = sd(survCurve, na.rm = F) / sqrt(sum(!is.na(survCurve))),
                   min = mu- se, max = mu + se) %>%
  ggplot(aes(time, mu, color = condition, fill = condition)) + geom_line() +
  geom_ribbon(aes(time, ymin = min, ymax = max), alpha = 0.5, color = NA) +
  geom_line(data = optim, aes(t, surv, color = condition, linetype = condition, alpha = condition), size = 1.2) +
  geom_line(data = data.frame(t = kmGrid[kmGrid > 2],surv = 1),
            aes(t, surv), color = conditionColors[1], size = 1.2, inherit.aes = F, alpha = 0.8) + 
  geom_point(data = sigData, aes(t, isSig), inherit.aes = F, color = "black", shape = 4, size = 0.8) + 
  scale_fill_manual(values = conditionColors) +
  scale_color_manual(values = conditionColors) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_alpha_manual(values = c(0.8, 1))+
  xlab("Elapsed time (s)") + ylab("Survival rate") + myTheme +
  theme(legend.position = "none") 
ggsave("figures/MFPlot/survival_curve.eps", width = 4, height = 4)
ggsave("figures/MFPlot/survival_curve.png", width = 4, height = 4) 
