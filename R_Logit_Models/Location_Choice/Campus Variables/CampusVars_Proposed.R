# ---- Packages ----
library(mlogit)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df$School_Codes = as.factor(df$School_Codes)
mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Domestic = mldf$Domestic * 100

# ---- Run Model ----
model = mlogit(School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist | Level,
               data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
print(summary(model))
print(mean(fitted(model)))
