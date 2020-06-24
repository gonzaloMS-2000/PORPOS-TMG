library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Simple")

df <- read.csv("../Campus_Choice_Segments.csv")
df$Campus = as.factor(df$Campus)

ml_data = mlogit.data(df, varying=2:8, choice="Campus", shape="wide")

model = mlogit(Campus ~ Dist, data=ml_data, reflevel="0")
summary(model)

preds = vector()
x = fitted(model, outcome = FALSE)
for (i in 1:nrow(x))
{
  preds = c(preds, sample(7, 1, prob = x[i,]) - 1)
}
preds = as.factor(preds)
  
y = confusionMatrix(preds, df$Campus)
cm = y[[2]] # Access elements with y[i, j]
acc = y[[3]] # Accuracy, Kappa, Lower, Upper, Null, P-Val, McNemar

stats = y[[4]]
macro_prf = c(mean(stats[,5]), mean(stats[,6]), mean(stats[,7])) # Note: minor difference between f1 and 2PR / (P+R)
mcc = mcc(preds = preds, actuals = df$Campus) # Matthews

cat("Micro Accuracy\t", acc[1], "\nMacro Precision\t", macro_prf[1], "\nMacro Recall\t", macro_prf[2],
    "\nMacro F-1 Score\t", macro_prf[3], "\nMatthews Coeff\t", mcc, "\nAve Prob of Obs\t", mean(fitted(model)))