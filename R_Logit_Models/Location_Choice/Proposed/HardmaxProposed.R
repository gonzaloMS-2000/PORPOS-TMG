# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df$School_Codes = as.factor(df$School_Codes)
mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))

# ---- Simpler Model ----
num_trials = 1L
num_metrics = 5L # Accuracy, Macro PRF, MCC
metrics = array(numeric(), c(num_trials, num_metrics)) 
for (i in 1:1) {
  actuals = subset(df, Segment!=0)$School_Codes
  model = mlogit(School_Codes ~ Dist + Family:Dist + Enrol | 0,
                 data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
  probs = fitted(model, outcome = FALSE)
  for (j in 1:num_trials) {
    preds = vector()
    for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
    preds = as.factor(preds)
    y = confusionMatrix(preds, actuals)
    metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]), mcc(preds=preds, actuals=actuals))
  }
  print(metrics)
  print(summary(model))
  cat("APO:", mean(fitted(model)))
}
