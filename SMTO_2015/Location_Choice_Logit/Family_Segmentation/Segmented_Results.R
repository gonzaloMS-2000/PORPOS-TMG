# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Family_Segmentation")
df <- read.csv("../../Data/SMTO_2015/Formatted.csv")
df$School = as.factor(df$School)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 19:95)

# ---- Run Model ----
for (j in 0:1)
{
  actuals = subset(df, Family==j)$School
  model = mlogit(School ~ Dist + Total | 0, data=mldf, subset=mldf$Family==j)
  probs = fitted(model, outcome = FALSE)
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / 7
  f1 = sum(y[[4]][,7], na.rm=TRUE) / 7
  metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
  names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
  print(summary(model))
  print(metrics)
}
