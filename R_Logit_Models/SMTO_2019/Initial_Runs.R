# Packages ----
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
df <- read.csv("../../Data/SMTO_2019/SMTO_2019_Complete_Input.csv")
df$Liv_Arr = as.factor(df$Liv_Arr)
df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Work = as.factor(df$Work)
df$Licence = as.logical.factor(df$Liv_Arr)
df$Family = ifelse(df$Family == "True", 1, 0)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))
cols = setdiff(levels(df$School), unis)
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(17:70, 72:98))
mldf$Closest = as.integer(mldf$Closest & (mldf$Dist <= 2))

# Reference Model
model1 = mlogit(School ~ Dist + Enrol + Dist:Family | 0, data=mldf)
summary(model1)[[18]]
probs = as.data.frame(fitted(model1, outcome=FALSE))
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
y = confusionMatrix(preds, df$School)
y[[3]][1]
mean(fitted(model1))
summary(model1)[[2]]


# Reference Model (Closest <2km)
model2 = update(model1, School ~ Dist + Enrol + Dist:Family + Closest | 0)
summary(model2)[[18]]
probs = as.data.frame(fitted(model2, outcome=FALSE))
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
y = confusionMatrix(preds, df$School)
y[[3]][1]
mean(fitted(model2))
summary(model2)[[2]]


# ASC Model
model3 = mlogit(School ~ Dist + Dist:Family, data=mldf, reflevel='SG')
summary(model3)[[18]]
probs = as.data.frame(fitted(model3, outcome=FALSE))
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
y = confusionMatrix(preds, df$School)
y[[3]][1]
mean(fitted(model3))
summary(model3)[[2]]


# ASC Model (Closest <2km)
model4 = update(model3, School ~ Dist + Dist:Family + Closest)
summary(model4)[[18]]
probs = as.data.frame(fitted(model4, outcome=FALSE))
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
y = confusionMatrix(preds, df$School)
y[[3]][1]
mean(fitted(model4))
summary(model4)[[2]]
