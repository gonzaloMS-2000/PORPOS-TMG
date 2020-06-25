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
# model = mlogit(School_Codes ~ Dist, data=mldf, reflevel="YG", weights = mldf$Exp) # 0.270
# model = mlogit(School_Codes ~ AIVTT, data=mldf, reflevel="YG", weights = mldf$Exp) # 0.264
# model = mlogit(School_Codes ~ TPTT, data=mldf, reflevel="YG", weights = mldf$Exp) # 0.256

metrics = array(numeric(), c(5, 6))
output = array(numeric(), c(4, 6))
formulas = c(mFormula(School_Codes ~ Dist),
             mFormula(School_Codes ~ Dist + I(Family * Domestic)),
             mFormula(School_Codes ~ Dist + I(Family * Tuition)),
             mFormula(School_Codes ~ Dist + I(Family * Admission_Avg)))

for (k in 1:length(formulas))
{
  print(k)
  model = mlogit(formulas[[k]], data=mldf, reflevel="YG")
  x = fitted(model, outcome = FALSE)
  for (j in 1:5)
  {
    preds = vector()
    for (i in 1:nrow(x)) preds = c(preds, sample(7, 1, prob = x[i,]) - 1)
    preds = as.factor(preds)
    levels(preds) = levels(df$School_Codes)
    y = confusionMatrix(preds, df$School_Codes)
    metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]),
                    2 * mean(y[[4]][,5]) * mean(y[[4]][,6]) / (mean(y[[4]][,5]) + mean(y[[4]][,6])),
                    mcc(preds=preds, actuals=df$School_Codes), mean(fitted(model)))
  }
  output[k,] = apply(metrics, 2, mean)
}
output