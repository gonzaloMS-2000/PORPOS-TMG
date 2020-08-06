# Packages ----
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_Combined")
df <- read.csv("../../Data/SMTO_Combined_Complete_Input.csv")
df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Licence = ifelse(df$Licence == "True", 1, 0)
df$Family = ifelse(df$Family == "True", 1, 0)
# unis = levels(droplevels(subset(df, School_Type == 'University')$School))

mldf = mlogit.data(df, choice="School", shape="wide", varying = 16:96)
mldf$Closest = ifelse(mldf$Closest == "True", 1, 0)
mldf$Closest = (mldf$Closest == 1) & (mldf$Dist <= 2)

calls = c("School ~ Dist + log(Enrol) + Dist:Family | 0",
          "School ~ Dist + log(Enrol) + Dist:Family + Closest | 0")
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