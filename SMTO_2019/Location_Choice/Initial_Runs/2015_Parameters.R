# Packages ----
library(mlogit)
source("../../../Metrics.R")
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2019/Location_Choice/Initial_Runs")

# 2015 Model ----
df <- read.csv("../../../Data/SMTO_2015/Formatted.csv")
df$School = as.factor(df$School)
codes = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
for (code in codes) df[[paste0('IsClosest.', code)]] = as.integer((df[[paste0('Dist.', code)]] <= 2) & df[[paste0('Closest.', code)]])
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(19:95, 100:106))
model = mlogit(School ~ Dist + log(Total) + Family:Dist + IsClosest | 0, data=mldf)
summary(model)

# Calculate Utilities ----
B_Dist = coef(model)[[1]]
B_Enrol = coef(model)[[2]]
B_Closest = coef(model)[[3]]
B_Famdist = coef(model)[[4]]
mldf$Util = mldf$Dist * (B_Dist + mldf$Family * B_Famdist) + log(mldf$Total) * B_Enrol + B_Closest * mldf$IsClosest 

# Ensure Logsums Calculated Correctly ----
logsums = vector(length = nrow(df))
for (i in 1:nrow(df)){
  start = i * 7 - 6
  end = i * 7
  logsums[i] = log(sum(exp(mldf$Util[start:end])))
} 
x = logsum(model)
max(abs(logsums - x))

# 2019 Data ----
df <- read.csv("../../../Data/SMTO_2019/Formatted.csv")
actuals = df$School = as.factor(df$School)
real_levels = levels(actuals)
df$FamilyTrue = ifelse(df$Family == "True", 1, 0)
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(18:71, 73:99))
mldf$Closest = as.integer(mldf$Closest & (mldf$Dist <= 2))

# Calculate Utilities and Probabilities ----
mldf$Util = mldf$Dist * (B_Dist + mldf$FamilyTrue * B_Famdist) + log(mldf$Enrol) * B_Enrol + B_Closest * mldf$Closest 
probs = matrix(NA, nrow(df), 27)
for (i in 1:nrow(df)){
  start = i * 27 - 26
  end = i * 27
  probs[i, ] = exp(mldf$Util[start:end]) / sum(exp(mldf$Util[start:end]))
}
probs = as.data.frame(probs)
names(probs) = levels(df$School)

# Metrics ----
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
cm = softmax_cm(probs, actuals, real_levels)
sum(diag(cm))/sum(cm)
obs_probs = matrix(nrow(df))
for (i in 1:nrow(df)){
  obs_probs[i] = probs[df$School[i]][i,]
}
mean(obs_probs)
sum(log(obs_probs))

# Notes ----
# For logsums: logsum(coefs or model[[1]], X=model.matrix(model, mldf), formula=formula)
# For full probabilities: fitted(model, outcome=FALSE) or fitted(model, type="probabilities")
# For obs. probabilities: fitted(model)
# For individual parameters: fitted(model, type="parameters")