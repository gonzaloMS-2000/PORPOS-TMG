# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df = subset(df, Segment!=0 & Family==1)
df$School_Codes = as.factor(df$School_Codes)
df$Work = as.factor(df$Work)
df$Work = relevel(df$Work, "Unknown")
df$Status = as.factor(df$Status)
df$Status = relevel(df$Status, "FT")
df$Income = as.factor(df$Income)
df$Income = relevel(df$Income, "Unknown")
mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))
mldf$TPTT = mldf$TPTT/60
mldf$AIVTT = mldf$AIVTT/60

# ---- Variables ----
num_campuses = 7L
actuals = df$School_Codes
vars = c("Work", "Status", "Income")
for (var in vars) {
  formula = paste0("School_Codes ~ Dist + Enrol + Dist:", var, " | 0")
  model = mlogit(as.formula(formula), data=mldf, weights=mldf$Exp_Segment, reflevel="SG")
  probs = fitted(model, outcome = FALSE)
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / num_campuses
  f1 = sum(y[[4]][,7], na.rm=TRUE) / num_campuses
  metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
  names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
  cat(var, "\n")
  print(metrics)
  print(summary(model))
}


model = mlogit(School_Codes ~ Dist + Enrol + Licence:AIVTT | 0, data=mldf, weights=mldf$Exp_Segment, reflevel="SG")
# ---- One Run ----
probs = fitted(model, outcome = FALSE)
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
y =  suppressWarnings(confusionMatrix(preds, actuals))
prec = sum(y[[4]][,5], na.rm=TRUE) / num_campuses
f1 = sum(y[[4]][,7], na.rm=TRUE) / num_campuses
metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
summary(model)
metrics