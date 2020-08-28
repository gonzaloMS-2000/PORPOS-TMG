# Imports ----
library(mlogit)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015")
source("../Metrics.R")

# Load and Format Data ----
df <- read.csv("../Data/SMTO_2015/Formatted.csv")
actuals = df$School = as.factor(df$School)
real_levels = levels(actuals)
codes = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
for (code in codes) df[[paste0('IsClosest.', code)]] = df[[paste0('Closest.', code)]]
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(19:95, 100:106))
mldf$NonFamily = 1 - mldf$Family

# Run Models for Different Thresholds ----
ts = c(30, 20, 10, 5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1, 0.5, 0)
APOs = LLs = vector(length=length(ts))
for (i in 1:length(ts))
{
  print(ts[i])
  mldf$Closest = as.integer((mldf$Dist <= ts[i]) & (mldf$IsClosest == 1))
  model = mlogit(School ~ Dist + log(Total) + Family:Dist + Family:Closest | 0, data=mldf)
  APOs[i] = mean(fitted(model))
  LLs[i] = logLik(model)
}
APOs
LLs

# Selected Model ----
mldf$Closest = as.integer((mldf$IsClosest == 1) & (mldf$Dist <= ifelse(mldf$Family==1, 2.5, 2)))
model = mlogit(School ~ Dist + log(Total) + Family:Dist + Closest:Family + Closest:NonFamily | 0, data=mldf)
summary(model)

# Metrics ----
probs = get_probs(model)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(model)
get_log_lik(model)