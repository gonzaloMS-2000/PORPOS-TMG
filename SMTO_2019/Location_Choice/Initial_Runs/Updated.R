# Packages ----
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2019/Location_Choice/Initial_Runs")
df <- read.csv("../../../Data/SMTO_2019/Formatted.csv")
df$School = as.factor(df$School)
df$FamilyTrue = ifelse(df$Family == "True", 1, 0)
df$FamilyFalse = ifelse(df$Family == "False", 1, 0)
df$FamilyUnknown = ifelse(df$Family == "", 1, 0)
names(df)
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(18:71, 73:99))
mldf$Closest = as.integer(mldf$Closest & (mldf$Dist <= 2))

# Reference Model
model = mlogit(School ~ Dist + log(Enrol) + Dist:FamilyTrue + Closest | 0, data=mldf)
summary(model)
probs = as.data.frame(fitted(model, outcome=FALSE))
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
y = confusionMatrix(preds, df$School)
y[[3]][1]
mean(fitted(model))
summary(model)[[2]]
