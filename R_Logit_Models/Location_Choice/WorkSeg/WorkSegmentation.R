# ---- Packages ----
library(mlogit)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/WorkSeg")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df$School_Codes = as.factor(df$School_Codes)
df$Work = as.factor(df$Work)
mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Domestic = mldf$Domestic * 100

# ---- Full Models ----
formulas = c("School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Domestic| Level",
             "School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Domestic| Level + Work",
             "School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Domestic + Work:Dist | Level")
for (i in 1:length(formulas)) {
  model = mlogit(as.formula(formulas[[i]]), data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
  print(summary(model))
  print(mean(fitted(model)))
}             

# ---- Old Segmented ----
num_segments = 7L
formulas = c("School_Codes ~ Dist", "School_Codes ~ Dist | Work", "School_Codes ~ Dist + Work:Dist")
for (i in 1:length(formulas)) {
  for (j in 1:(num_segments-1)) {
    model = mlogit(as.formula(formulas[[i]]), data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment==j)
    print(mean(fitted(model)))
  }
}

# ---- New Segmented ----
for (i in 1:4) {
  model = mlogit(School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Domestic | Level,
                 data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=(mldf$Segment!=0 & mldf$Work==levels(mldf$Work)[i]))
  print(summary(model))
  print(mean(fitted(model)))
}




