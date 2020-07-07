# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/SocioEconomicVars")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
# factor_cols = c(1:7, 10, 14:17)
# df[factor_cols] = lapply(df[factor_cols], factor)
df$School_Codes = as.factor(df$School_Codes)
ml_data = mlogit.data(df, varying=18:87, choice="School_Codes", shape="wide")

# ---- Model Formulas ----
base_formula = "School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Level:Dist + Family:Status:Dist + Level:Status:Dist | Family + Level"
# socio_vars = c("Cars", "Adults", "Children", "Income", "Licence", "Work")
socio_vars = c("Children + Adults + Cars + Licence")

# ---- Prepare Output ----
num_segments = 7L
num_campuses = 7L
num_trials = 5L
num_metrics = 5L # Accuracy, Macro PRF, MCC
metrics = array(numeric(), c(num_trials, num_metrics)) 
output = array(numeric(), c(length(socio_vars) + 1, num_campuses + num_metrics + 2))

# ---- Ref. Model ----
model = mlogit(as.formula(base_formula), data=ml_data, reflevel="SG", subset = ml_data$Segment!=0, weights=ml_data$Exp_Segment)
x = fitted(model, outcome = FALSE)
for (j in 1:num_campuses) output[1, j] = model[[1]][[j]]
for (j in 1:num_trials) {
  preds = vector()
  for (k in 1:nrow(x)) preds = c(preds, colnames(x)[sample(7, 1, prob = x[k,])])
  preds = as.factor(preds)
  actuals = subset(df, Segment!=0)$School_Codes
  y = confusionMatrix(preds, actuals)
  metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]), mcc(preds=preds, actuals=actuals))
}
output[1, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
output[1, (num_campuses+num_metrics+1):(num_campuses+num_metrics+2)] = c(mean(fitted(model)), summary(model)[[20]][[1]])


# ---- Model Runs ----
for (i in 1:length(socio_vars)) {
  print(i)
  model = mlogit(as.formula(paste(base_formula, socio_vars[i], sep = " + ")), data=ml_data, reflevel="SG", subset = ml_data$Segment!=0, weights=ml_data$Exp_Segment)
  x = fitted(model, outcome = FALSE)
  for (j in 1:num_campuses) output[i+1, j] = model[[1]][[j]]
  for (j in 1:num_trials) {
    preds = vector()
    for (k in 1:nrow(x)) preds = c(preds, colnames(x)[sample(7, 1, prob = x[k,])])
    preds = as.factor(preds)
    actuals = subset(df, Segment!=0)$School_Codes
    y = confusionMatrix(preds, actuals)
    metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]), mcc(preds=preds, actuals=actuals))
  }
  output[i+1, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
  output[i+1, (num_campuses+num_metrics+1):(num_campuses+num_metrics+2)] = c(mean(fitted(model)), summary(model)[[20]][[1]])
}  
output  
# ---- Print Output ----
#print(output)
c("ASC_MI", "ASC_OC", "ASC_RY", "ASC_SC", "ASC_SG", "ASC_YK",
  "B_DIST", "Accuracy", "MacroPre", "MacroRec", "MacroF1",
  "Matthews", "AvePrObs", "McF. R^2")
write.table(output, "Next_Steps_Results.csv", sep=",", na="")