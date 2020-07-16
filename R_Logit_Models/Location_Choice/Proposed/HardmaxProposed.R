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

# ---- Variables ----
num_campuses = 7L
num_metrics = 5L # Accuracy, Macro PRF, MCC
actuals = subset(df, Segment!=0)$School_Codes
preds = vector()

# ---- Run Model ----
model = mlogit(School_Codes ~ Dist + Family:Dist + Enrol | 0, data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
probs = fitted(model, outcome = FALSE)

# ---- Calculate Metrics ----
for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
y =  suppressWarnings(confusionMatrix(preds, actuals))
prec = sum(y[[4]][,5], na.rm=TRUE) / num_campuses
f1 = sum(y[[4]][,7], na.rm=TRUE) / num_campuses
metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")

# ---- Print Results ----
metrics
