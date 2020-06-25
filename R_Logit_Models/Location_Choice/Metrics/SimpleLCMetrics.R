# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Metrics")
df <- read.csv("../Campus_Choice_Segments.csv")
df$Campus = as.factor(df$Campus)
ml_data = mlogit.data(df, varying=2:8, choice="Campus", shape="wide")

# ---- Prepare Output ----
num_segments = 7L
num_campuses = 7L
num_trials = 5L
num_metrics = 6L # Accuracy, Macro PRF, MCC, APO
metrics = array(numeric(), c(num_trials, num_metrics)) 
output = array(numeric(), c(num_segments + 1, num_campuses + num_metrics + 1))

# ---- Reference Model ----
model = mlogit(Campus ~ Dist, data=ml_data, reflevel="0")
for (j in 1:num_campuses) output[1, j] = model[[1]][[j]]
x = fitted(model, outcome = FALSE)
for (j in 1:num_trials)
{
  preds = vector()
  for (i in 1:nrow(x)) preds = c(preds, sample(7, 1, prob = x[i,]) - 1)
  preds = as.factor(preds)
  y = confusionMatrix(preds, df$Campus)
  metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]),
                  mcc(preds=preds, actuals=df$Campus), mean(fitted(model)))
}
output[1, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)

# ---- Segmented Model ----
for (i in 1:num_segments)
{
  model = mlogit(Campus~Dist, data=ml_data, reflevel="0", subset=ml_data$Segment==(i-1))
  for (j in 1:num_campuses) output[i+1, j] = model[[1]][[j]]
  
  x = fitted(model, outcome = FALSE)
  for (j in 1:num_trials)
  {
    preds = vector()
    for (k in 1:nrow(x)) preds = c(preds, sample(7, 1, prob = x[k,]) - 1)
    preds = as.factor(preds) # TODO manually set levels to correspond to df$Campus
    levels(preds) = levels(df$Campus)
    y = confusionMatrix(preds, subset(df, Segment == i-1)$Campus)
    y[[4]][,5][is.na(y[[4]][,5])] = 0
    y[[4]][,7][is.na(y[[4]][,7])] = 0
    
    metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]),
                    mcc(preds=preds, actuals=subset(df, Segment == i-1)$Campus),
                    mean(fitted(model)))
}
  output[i+1, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
}

# ---- Print Output ----
print(output)
write.table(output, "MetricsOutput.csv", sep=",", na="", row.names = c("All", 0:6),
            col.names = c("Segment", "ASC_SC", "ASC_MI", "ASC_YK", "ASC_YG", "ASC_RY", "ASC_OC",
                          "B_DIST", "Accuracy", "MacroPre", "MacroRec", "MacroF1",
                          "Matthews", "AvePrObs"))