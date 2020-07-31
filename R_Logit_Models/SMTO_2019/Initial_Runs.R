# Packages ----
library(mlogit)
library(data.table)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
df <- read.csv("../../Data/SMTO_2019/SMTO_2019_Complete_Input.csv")
df$Liv_Arr = as.factor(df$Liv_Arr)
df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Work = as.factor(df$Work)
df$Licence = as.logical.factor(df$Liv_Arr)
df$Family = ifelse(df$Family, 1, 0)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))
cols = setdiff(levels(df$School), unis)
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(17:70, 72:98))
mldf$Closest = as.integer(mldf$Closest & (mldf$Dist <= 2))

# Reference Model
model1 = mlogit(School ~ Dist + Enrol + Dist:Family | 0, data=mldf, reflevel='SG')
summary(model1)[[18]]
mean(fitted(model1))

# Reference Model (Closest <2km)
model2 = update(model1, School ~ Dist + Enrol + Dist:Family + Closest | 0)
summary(model2)[[18]]
mean(fitted(model2))