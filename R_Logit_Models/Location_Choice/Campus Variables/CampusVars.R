# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
factor_cols = c(1:7, 13:15, 17)
df[factor_cols] = lapply(df[factor_cols], factor)
df$Family = ifelse(df$Family == "Family", 1, 0)

mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Domestic = mldf$Domestic * 100
mldf$Tuition = mldf$Tuition / 100
mldf$Admission_Avg = mldf$Admission_Avg * 100

# ---- Run Models ----

# TODO Run for all segments, output to file
# F1, MCC, APO for formulas 1-4
# Use weights by segments - fix them first
# Interaction with Family won't add value?


# formulas = c(mFormula(School_Codes ~ Dist),
#              mFormula(School_Codes ~ Dist + I(Family * Domestic)),
#              mFormula(School_Codes ~ Dist + I(Family * Tuition)),
#              mFormula(School_Codes ~ Dist + I(Family * Admission_Avg)),
#              mFormula(School_Codes ~ Dist + I(Family * Domestic) + I(Family * Tuition)),
#              mFormula(School_Codes ~ Dist + I(Family * Domestic) + I(Family * Admission_Avg)),
#              mFormula(School_Codes ~ Dist + I(Family * Tuition) + I(Family * Admission_Avg)),
#              mFormula(School_Codes ~ Dist + I(Family * Domestic) + I(Family * Admission_Avg) + I(Family * Tuition)))
formulas = c(mFormula(School_Codes ~ Dist + I(LogIncome * Tuition)))
metrics = array(numeric(), c(5, 6))
output = array(numeric(), c(length(formulas), 6))

for (k in 1:length(formulas))
{
  print(k)
  model = mlogit(formulas[[k]], data=mldf, reflevel="MI", weights=mldf$Exp_Level)
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


# ---- Segments ----
num_segments = 7L
# formulas = c(mFormula(School_Codes ~ Dist + AIVTT),
#              mFormula(School_Codes ~ AIVTT + TPTT),
#              mFormula(School_Codes ~ TPTT + Dist))
formulas = c(mFormula(School_Codes ~ Dist + AIVTT + TPTT))
metrics = array(numeric(), c(5, 6))
output = array(numeric(), c(length(formulas) * num_segments, 6))

for (i in 0:(num_segments-1))
{
  print(i)
  for (formula in formulas)
  {
    model = mlogit(formula, data=mldf, reflevel="YG", subset = mldf$Segment == i)
    print(summary(model)[[20]][1])
  }
}

    # x = fitted(model, outcome = FALSE)
    # for (j in 1:5)
    # {
    #   preds = vector()
    #   for (l in 1:nrow(x)) preds = c(preds, sample(7, 1, prob = x[l,]) - 1)
    #   preds = as.factor(preds)
    #   levels(preds) = levels(subset(df, Segment == i)$School_Codes)
    #   y = confusionMatrix(preds, subset(df, Segment == i)$School_Codes)
    #   metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]),
    #                   2 * mean(y[[4]][,5]) * mean(y[[4]][,6]) / (mean(y[[4]][,5]) + mean(y[[4]][,6])),
    #                   mcc(preds=preds, actuals=subset(df, Segment == i)$School_Codes), mean(fitted(model)))
    # }
    # output[i * 3 + k,] = apply(metrics, 2, mean)
#   }
# }


