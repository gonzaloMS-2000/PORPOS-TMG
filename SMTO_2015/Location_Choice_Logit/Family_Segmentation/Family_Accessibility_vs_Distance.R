# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Proposed")
df <- read.csv("AccessibilityData.csv")
df = subset(df, Segment != 0)
df$School_Codes = as.factor(df$School_Codes)
mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = c(18:87, 93:99))
mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))

# ---- Variables ----
num_campuses = 7L
num_metrics = 5L # Accuracy, Macro PRF, MCC

# ---- Metrics Calculations ----
get_metrics <- function(model, actuals) {
  probs = fitted(model, outcome=FALSE)
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / 7
  f1 = sum(y[[4]][,7], na.rm=TRUE) / 7
  metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)) * length(fitted(model)) / length(actuals))
  names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
  return(metrics)
}

# ---- Non-Family Model ----
actuals = subset(df, Family==0)$School_Codes
model = mlogit(School_Codes ~ Dist + Enrol | 0, data=mldf, weights=mldf$Exp_Segment,
               reflevel="SG", subset= mldf$Family==0)
summary(model)
metrics = get_metrics(model, actuals)
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
metrics

# ---- Family Model D ----
actuals = subset(df, Family==1)$School_Codes
model = mlogit(School_Codes ~ Dist + Enrol | 0, data=mldf, weights=mldf$Exp_Segment,
               reflevel="SG", subset= mldf$Family==1)
summary(model)
metrics = get_metrics(model, actuals)
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
metrics

# ---- Family Model A ----
actuals = subset(df, Family==1)$School_Codes
model = mlogit(School_Codes ~ Access + Enrol | 0, data=mldf, weights=mldf$Exp_Segment,
               reflevel="SG", subset= mldf$Family==1)
summary(model)
metrics = get_metrics(model, actuals)
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
metrics

# ---- Family Model A+D ----
actuals = subset(df, Family==1)$School_Codes
model = mlogit(School_Codes ~ Access + Enrol + Dist | 0, data=mldf, weights=mldf$Exp_Segment,
               reflevel="SG", subset= mldf$Family==1)
summary(model)
metrics = get_metrics(model, actuals)
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
metrics