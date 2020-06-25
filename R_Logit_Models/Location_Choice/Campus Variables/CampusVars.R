# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")

factor_cols = c(1:7, 9, 13:16)
df[factor_cols] = lapply(df[factor_cols], factor)

mldf = mlogit.data(df, varying=17:23, choice="School_Codes", shape="wide")

head(df[17:86])
length(17:86)
