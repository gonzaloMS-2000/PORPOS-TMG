# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")

factor_cols = c(1:7, 13:16)
df[factor_cols] = lapply(df[factor_cols], factor)
df$Family = ifelse(df$Family == "Family", 1, 0)
# df$Income = ifelse(df$Income == "High", 2, ifelse(df$Income == "Low", 1, 1.266)) # Average for unknown

mldf = mlogit.data(df, varying=17:86, choice="School_Codes", shape="wide")
mldf$Domestic = mldf$Domestic * 100
mldf$Tuition = mldf$Tuition / 100
mldf$Admission_Avg = mldf$Admission_Avg * 100

# ---- Run Models ----
model = mlogit(School_Codes ~ Dist, data=mldf, reflevel="YG", weights = mldf$Exp) # 0.270
model = mlogit(School_Codes ~ AIVTT, data=mldf, reflevel="YG", weights = mldf$Exp) # 0.264
model = mlogit(School_Codes ~ TPTT, data=mldf, reflevel="YG", weights = mldf$Exp) # 0.256

model = mlogit(School_Codes ~ Dist, data=mldf, reflevel="YG")                             #          0.126
model = mlogit(School_Codes ~ Dist + I(Family * Domestic), data=mldf, reflevel="YG")      #  0.076 | 0.143
model = mlogit(School_Codes ~ Dist + I(Family * Tuition), data=mldf, reflevel="YG")       # -0.145 | 0.134
model = mlogit(School_Codes ~ Dist + I(Family * Admission_Avg), data=mldf, reflevel="YG") # -0.062 | 0.128
summary(model)
