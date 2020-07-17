# ---- Packages ----
library(mlogit)
library(caret)
library(mltools)
library(data.table)


# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Proposed")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df = subset(df, Segment!=0)
df$School_Codes = as.factor(df$School_Codes)

# ---- Prepare Outputs ----
num_metrics = 6
num_folds = 10
results = matrix(data = NA, nrow = num_folds, ncol = num_metrics)

# ---- Model with k-Fold ----
df <- df[sample(nrow(df)),]
folds <- cut(seq(1, nrow(df)), breaks=num_folds, labels=FALSE)
for(i in 1:num_folds){
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData <- df[testIndexes, ]
  trainData <- df[-testIndexes, ]
  mldf = mlogit.data(trainData, choice="School_Codes", shape="wide", varying = 18:87)
  mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))
  test_mldf = mlogit.data(testData, choice="School_Codes", shape="wide", varying = 18:87)
  test_mldf$Enrol = ifelse(test_mldf$Level == "UG", test_mldf$UG, ifelse(test_mldf$Level == "Grad", test_mldf$Grad, test_mldf$Total))
  model = mlogit(School_Codes ~ Dist + Family:Dist + Enrol | 0, data=mldf, reflevel="SG")
  actuals = testData$School_Codes
  probs = predict(model, newdata = test_mldf)
  preds = vector()
  for (j in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[j,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / 7
  f1 = sum(y[[4]][,7], na.rm=TRUE) / 7
  results[i, ] = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)) * length(fitted(model)) / nrow(df))
}

results_summary = colMeans(results)
names(results_summary) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
results_summary

# ---- Original Model ----
full_mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
full_mldf$Enrol = ifelse(full_mldf$Level == "UG", full_mldf$UG, ifelse(full_mldf$Level == "Grad", full_mldf$Grad, full_mldf$Total))
model = mlogit(School_Codes ~ Dist + Family:Dist + Enrol | 0, data=full_mldf, reflevel="SG")
actuals = df$School_Codes
probs = fitted(model, outcome=FALSE)
preds = vector()
for (j in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[j,])[[1]]])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
y =  suppressWarnings(confusionMatrix(preds, actuals))
prec = sum(y[[4]][,5], na.rm=TRUE) / 7
f1 = sum(y[[4]][,7], na.rm=TRUE) / 7
metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
metrics
