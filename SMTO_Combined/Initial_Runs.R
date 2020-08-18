# Packages ----
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_Combined")
df <- read.csv("../../Data/SMTO_Combined_Complete_Input.csv")
rf_df = read.csv("../../Random_Forest/2019 UniCollege Models/Combined_RF_Predictions.csv", row.names=1)
df = cbind(df, rf_df)
df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Licence = ifelse(df$Licence == "True", 1, 0)
df$Family = ifelse(df$Family == "True", 1, 0)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))

mldf = mlogit.data(df, choice="School", shape="wide", varying = 16:96)
mldf$Closest = ifelse(mldf$Closest == "True", 1, 0)
mldf$Closest = (mldf$Closest == 1) & (mldf$Dist <= 2)
mldf$IsUni = ifelse(mldf$alt %in% unis, 1, 0)
mldf$IsCol = 1 - mldf$IsUni
mldf$IsPredType = ifelse((mldf$IsUni == 1) & (mldf$Pred == "Uni") | (mldf$IsCol == 1) & (mldf$Pred == "Col"), 1, 0)
mldf$IsType = ifelse(mldf$alt %in% unis, mldf$Prob.Uni, mldf$Prob.Col)

# calls = c("School ~ Dist + log(Enrol) + Dist:Family | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsCol:Prob.Col | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsUni:Prob.Uni | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsPredType | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsCol:Prob.Col + IsUni:Prob.Uni | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + Closest | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsCol:Prob.Col + Closest | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsUni:Prob.Uni + Closest | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsPredType + Closest | 0",
#           "School ~ Dist + log(Enrol) + Dist:Family + IsCol:Prob.Col + IsUni:Prob.Uni + Closest | 0")
calls = c("School ~ Dist + log(Enrol) + Dist:Family + IsType | 0",
          "School ~ Dist + log(Enrol) + Dist:Family + IsType + Closest | 0")
for (call in calls){
  model = mlogit(as.formula(call), data=mldf)
  cat("\nCall:", call, "\n")
  print(summary(model)[[18]])
  probs = as.data.frame(fitted(model, outcome=FALSE))
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, names(probs)[which.max(probs[i,])])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
  y = suppressWarnings(confusionMatrix(preds, df$School))
  cat("Hardmax Accuracy (ACC):", y[[3]][1], "\n")
  cat("Softmax Accuracy (APO):", mean(fitted(model)), "\n")
  cat("Log likelihood:", logLik(model)[[1]], "\n")
}