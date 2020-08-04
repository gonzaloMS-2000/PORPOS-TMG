# ---- Packages ----
library(mlogit)
library(caret)
library(mltools)
library(data.table)


# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Proposed")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df = subset(df, Segment!=0)
df$School = as.factor(df$School)

mldf = mlogit.data(df, choice="School", shape="wide", varying = c(18:87 , 97, 96, 93, 99, 98, 95, 94))
mldf$Closest = ifelse((mldf$Closest == 1) & (mldf$Dist <= 2), 1, 0)

model = mlogit(School ~ Dist + Total + Family:Dist + Closest | 0, data=mldf)
summary(model)

actuals = df$School
probs = fitted(model, outcome=FALSE)
preds = vector()
for (j in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[j,])[[1]]])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
y =  suppressWarnings(confusionMatrix(preds, actuals))
prec = sum(y[[4]][,5], na.rm=TRUE) / 7
f1 = sum(y[[4]][,7], na.rm=TRUE) / 7
c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
c("Acc", "Prec", "Rec", "F1", "MCC", "APO")