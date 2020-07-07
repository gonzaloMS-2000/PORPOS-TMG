# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/SocioEconomicVars")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df$School_Codes = as.factor(df$School_Codes)
df$Income = relevel(as.factor(df$Income), "Unknown")
df$Work = relevel(as.factor(df$Work), "Unknown")
df$Segment = as.factor(df$Segment)
ml_data = mlogit.data(df, varying=18:87, choice="School_Codes", shape="wide")

# ---- Formulas ----
formulas = c(mFormula(School_Codes ~ Dist | 1),
             mFormula(School_Codes ~ Dist | Work),
             mFormula(School_Codes ~ Dist | Work + Cars),
             mFormula(School_Codes ~ Dist | Cars + Adults + Children + Income + Licence + Work))
alt_formula = mFormula(School_Codes ~ Dist | Cars + Adults + Income + Licence + Work)

# ---- Prepare Output ----
num_segments = 6L
num_campuses = 7L
num_trials = 5L
num_metrics = 5L # Accuracy, Macro PRF, MCC
metrics = array(numeric(), c(num_trials, num_metrics)) 
output = array(numeric(), c((num_segments) * length(formulas), num_campuses + num_metrics + 2))

# --- Run Models ----
for (i in 1:length(formulas)) {
  for (j in 1:num_segments) {
    cat(i, ", ", j)
    actuals = subset(df, Segment==j)$School_Codes
    if (i==length(formulas) & j==6) model = mlogit(alt_formula, data=ml_data, reflevel="SG", subset = ml_data$Segment==j, weights=ml_data$Exp_Segment)
    else model = mlogit(formulas[[i]], data=ml_data, reflevel="SG", subset = ml_data$Segment==j, weights=ml_data$Exp_Segment)
    x = fitted(model, outcome = FALSE)
    for (k in 1:num_campuses) output[(i-1)*num_segments + j, k] = model[[1]][[j]]
    for (k in 1:num_trials) {
      preds = vector()
      for (l in 1:nrow(x)) preds = c(preds, colnames(x)[sample(7, 1, prob = x[l,])])
      preds = as.factor(preds)
      y = confusionMatrix(preds, actuals)
      metrics[k,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]), mcc(preds=preds, actuals=actuals))
    }
    output[(i-1)*num_segments + j, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
    output[(i-1)*num_segments + j, (num_campuses+num_metrics+1):(num_campuses+num_metrics+2)] = c(mean(fitted(model)), summary(model)[[20]][[1]])
  }
}  

# ---- Print Output ----
cols = c("ASC_MI", "ASC_OC", "ASC_RY", "ASC_SC", "ASC_SG", "ASC_YK",
         "B_DIST", "Accuracy", "MacroPre", "MacroRec", "MacroF1",
         "Matthews", "AvePrObs", "McF. R^2")
write.table(output, sep=",", na="", col.names=cols)
