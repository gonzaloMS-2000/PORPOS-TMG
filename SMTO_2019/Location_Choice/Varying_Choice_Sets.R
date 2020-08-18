# Packages ----
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
df <- read.csv("../../Data/SMTO_2019/SMTO_2019_Complete_Input.csv")
df$School = as.factor(df$School)
df$Family = ifelse(df$Family == "True", 1, 0)
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(17:70, 72:98))

mldf$av = ifelse(mldf$alt == 'CPI' & mldf$Dist > 70, 0, 1)
table(mldf$av, mldf$School)

# Run Model ----
model = mlogit(School ~ Dist + Enrol + Dist:Family | 0, data=mldf, subset=mldf$av==1)

# Calculate Utilities ----
B_Dist = coef(model)[[1]]
B_Enrol = coef(model)[[2]]
B_Famdist = coef(model)[[3]]
mldf$Util = ifelse(mldf$av == 1,
                   mldf$Dist * (B_Dist + mldf$Family * B_Famdist) + mldf$Enrol * B_Enrol,
                   -Inf)

# Generate Logsums ----
logsums = vector(length = nrow(df))
for (i in 0:0){
  start = i * 27 + 1
  end = (i + 1) * 27
  logsums[i+1] = log(sum(exp(mldf$Util[start:end])))
} 
x = logsum(model)
max(abs(logsums - x))

# Probabilities ----
probs_from_utils = matrix(NA, nrow(df), 27)
for (i in 0:(nrow(df) -1)){
  start = i * 27 + 1
  end = (i + 1) * 27
  probs_from_utils[i + 1, ] = exp(mldf$Util[start:end]) / exp(x[i + 1])
} 
z = fitted(model, outcome=FALSE)
max(abs(probs_from_utils - z))

# Calculate Utilities ----
B_Dist = -0.1371
B_Enrol = 1.0063
B_Famdist = 0.0662
mldf$Util_2015 = mldf$Dist * (B_Dist + mldf$Family * B_Famdist) + mldf$Enrol * B_Enrol

# Generate Logsums ----
logsums_2015 = vector(length = nrow(df))
for (i in 0:(nrow(df) -1)){
  start = i * 27 + 1
  end = (i + 1) * 27
  logsums_2015[i+1] = log(sum(exp(mldf$Util_2015[start:end])))
} 

# Probabilities ----
probs_from_utils_2015 = matrix(NA, nrow(df), 27)
for (i in 0:(nrow(df) -1)){
  start = i * 27 + 1
  end = (i + 1) * 27
  probs_from_utils_2015[i + 1, ] = exp(mldf$Util_2015[start:end]) / exp(x[i + 1])
} 
probs_from_utils_2015 = as.data.frame(probs_from_utils_2015)

campus_names = names(as.data.frame(fitted(model, outcome=FALSE)))
preds = vector()
for (i in 1:nrow(probs_from_utils_2015)) preds = c(preds, campus_names[which.max(probs_from_utils_2015[i,])])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(df$School), levels(preds)))
y = suppressWarnings(confusionMatrix(preds, df$School))
cat("Hardmax Accuracy (ACC):", y[[3]][1], "\n")

# Notes ----
# For logsums: logsum(coefs or model[[1]], X=model.matrix(model, mldf), formula=formula)
# For full probabilities: fitted(model, outcome=FALSE) or fitted(model, type="probabilities")
# For obs. probabilities: fitted(model)
# For individual parameters: fitted(model, type="parameters")