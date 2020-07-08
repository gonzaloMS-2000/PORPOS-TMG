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
mldf$Admission_Avg = mldf$Admission_Avg * 100
mldf$Domestic = mldf$Domestic * 100

# ---- Formulas ----
base_formula_1 = "School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Level:Dist + Family:Status:Dist + Level:Status:Dist"
formulas = c("+ Family:Admission_Avg | Family + Level", "+ Family:Admission_Avg | Family", "+ Family:Admission_Avg | Level",
             "+ Family:Domestic + Family:Tuition | Family + Level", "+ Family:Domestic + Family:Tuition | Family", "+ Family:Domestic + Family:Tuition | Level")

# ---- Prepare Output ----
output = array(numeric(), c(length(formulas), 3))
models = c()

# ---- Run Models ----
for (k in 6:length(formulas)) {
  print(k)
  model = mlogit(as.formula(paste(base_formula_1, formulas[[k]])), data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
  output[k,] = c(formulas[[k]], summary(model)[[20]][1], mean(fitted(model)))
  models = c(models, model)
}

# ---- Write Output ----
output[,1] = formulas
output
write.table(output, sep=",")

# Income Ideas
# Income: 0/1/2, means, log_means
# Tuition:Income, I(Tuition/Income), Admission:Avg_Income