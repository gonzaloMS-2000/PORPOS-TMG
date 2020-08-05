# Packages ----
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
df <- read.csv("../../Data/SMTO_2019/SMTO_2019_Complete_Input.csv")
rf_df = read.csv("../../Random_Forest/2019 UniCollege Models/RF_Predictions.csv", row.names=1)
df = cbind(df, rf_df)

# Format Data ----
df$School = as.factor(df$School)
df$Family = ifelse(df$Family == "True", 1, 0)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))

# Transform Data ----
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(17:70, 72:98))
mldf$Closest = as.integer(mldf$Closest & (mldf$Dist <= 2))
mldf$IsUni = ifelse(mldf$alt %in% unis, 1, 0)
mldf$IsCol = 1 - mldf$IsUni
mldf$IsPredType = ifelse((mldf$IsUni == 1) & (mldf$Pred == "Uni") | (mldf$IsCol == 1) & (mldf$Pred == "Col"), 1, 0)

# Run Models ----
calls = c("School ~ Dist + Enrol + Dist:Family | 0",
          "School ~ Dist + Enrol + Dist:Family + IsPredType | 0",
          "School ~ Dist + Enrol + Dist:Family + IsCol:Prob.Col | 0",
          "School ~ Dist + Enrol + Dist:Family + IsCol:Prob.Col + IsUni:Prob.Uni | 0")
for (call in calls){
  model = mlogit(as.formula(call), data=mldf)
  cat("Call:", call, "\n")
  print(summary(model)[[18]])
  cat("Log likelihood:", summary(model)[[2]], "\n")
  cat("Softmax Accuracy (APO):", mean(fitted(model)), "\n")
  probs = as.data.frame(fitted(model, outcome=FALSE))
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
  y = confusionMatrix(preds, df$School)
  cat("Hardmax Accuracy (ACC):", y[[3]][1], "\n\n")
}