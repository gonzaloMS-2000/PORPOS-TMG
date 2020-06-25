# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Data ----
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Location_Choice/SocioEconomicVars")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
levels(df$School_Codes) = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
df$School_Codes = as.factor(df$School_Codes)
ml_data = mlogit.data(df, varying=17:86, choice="School_Codes", shape="wide")

# ---- Prepare Output ----
num_segments = 7L
num_campuses = 7L
num_trials = 5L
num_metrics = 6L # Accuracy, Macro PRF, MCC, APO
metrics = array(numeric(), c(num_trials, num_metrics)) 
output = array(numeric(), c(num_segments + 1, num_campuses + num_metrics + 1))

# ---- Reference Model ----
model = mlogit(School_Codes ~ Dist, data=ml_data, reflevel="YG")
for (j in 1:num_campuses) {output[1, j] = model[[1]][[j]]}
x = fitted(model, outcome = FALSE)
for (j in 1:num_trials)
{
  preds = vector()
  for (i in 1:nrow(x)) preds = c(preds, sample(7, 1, prob = x[i,]) - 1)
  preds = as.factor(preds)
  levels(preds) = levels(df$School_Codes)
  y = confusionMatrix(preds, df$School_Codes)
  metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]),
                  mcc(preds=preds, actuals=df$School_Codes), mean(fitted(model)))
}
output[1, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)

# ---- Segmented Model ----
for (i in 1:num_segments)
{
  print(1)
  model = mlogit(School_Codes~ Dist | Income , data=ml_data, reflevel="YG", subset=ml_data$Segment==(i-1))
  print(2)
  print(summary(model))
  print(3)
  for (j in 1:num_campuses) output[i+1, j] = model[[1]][[j]]
  print(4)
  
  x = fitted(model, outcome = FALSE)
  print(5)
  for (j in 1:num_trials)
  {
    print(6)
    preds = vector()
    print(7)
    for (k in 1:nrow(x)) preds = c(preds, sample(7, 1, prob = x[k,]) - 1)
    print(8)
    preds = as.factor(preds) # TODO manually set levels to correspond to df$Campus
    print(9)
    levels(preds) = levels(df$School_Codes)
    print(10)
    y = confusionMatrix(preds, subset(df, Segment == i-1)$School_Codes)
    print(11)
    y[[4]][,5][is.na(y[[4]][,5])] = 0
    print(12)
    y[[4]][,7][is.na(y[[4]][,7])] = 0
    print(13)
    
    metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]),
                    mcc(preds=preds, actuals=subset(df, Segment == i-1)$School_Codes),
                    mean(fitted(model)))
  }
  output[i+1, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
}

# ---- Print Output ----
print(output)
# write.table(output, "MetricsOutput_Gonzalo.csv", sep=",", na="", row.names = c("All", 0:6),
#             col.names = c("Segment", "ASC_SC", "ASC_MI", "ASC_YK", "ASC_YG", "ASC_RY", "ASC_OC",
#                           "B_DIST", "Accuracy", "MacroPre", "MacroRec", "MacroF1",
#                           "Matthews", "AvePrObs"))