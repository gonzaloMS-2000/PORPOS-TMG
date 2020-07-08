# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)

# ---- Data ----
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/ArcGIS Test")
df <- read.csv("SMTO_2015_Complete_Input_NoOthers.csv")
homes_df <- read.csv("AllZoneDistances_Input.csv")
df$School_Codes = as.factor(df$School_Codes)
df$Segment = as.factor(df$Segment)
ml_data = mlogit.data(df, varying=18:24, choice="School_Codes", shape="wide")

# Formulas
formulas = list(c(mFormula(School_Codes ~ Dist | 1), 0))
                
# formulas = list(c(mFormula(School_Codes ~ Dist | 1), 0),
#                 c(mFormula(School_Codes ~ Dist | Family), 1),
#                 c(mFormula(School_Codes ~ Dist | Level), 1),
#                 c(mFormula(School_Codes ~ Dist | Status), 1),
#                 c(mFormula(School_Codes ~ Dist | Family + Level), 2),
#                 c(mFormula(School_Codes ~ Dist | Family + Status), 2),
#                 c(mFormula(School_Codes ~ Dist | Segment), 1),
#                 c(mFormula(School_Codes ~ Dist | Family + Status + Level), 3))

# formulas = list(c(mFormula(School_Codes ~ Dist | 1), 0),
#                 c(mFormula(School_Codes ~ Dist | Licence), 1),
#                 c(mFormula(School_Codes ~ Dist | Cars), 1),
#                 c(mFormula(School_Codes ~ Dist | Licence + Cars), 2),
#                 c(mFormula(School_Codes ~ Dist | Income), 1),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars), 2),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Licence), 3),
#                 c(mFormula(School_Codes ~ Dist | Income + Children), 2),
#                 c(mFormula(School_Codes ~ Dist | Income + Adults), 2),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Children), 3),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Adults), 3),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Licence + Children), 4),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Licence + Adults), 4),
#                 c(mFormula(School_Codes ~ Dist | Income + Children + Adults), 3),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Children + Adults), 4),
#                 c(mFormula(School_Codes ~ Dist | Income + Cars + Licence + Children + Adults), 5))

# formulas = list(c(mFormula(School_Codes ~ Dist | Family), 1),
#                 c(mFormula(School_Codes ~ Dist | Level), 1),
#                 c(mFormula(School_Codes ~ Dist | Status), 1),
#                 c(mFormula(School_Codes ~ Dist | Family + Level), 2),
#                 c(mFormula(School_Codes ~ Dist | Family + Status), 2),
#                 c(mFormula(School_Codes ~ Dist | Income), 1),
#                 c(mFormula(School_Codes ~ Dist | Cars), 1),
#                 c(mFormula(School_Codes ~ Dist | Licence + Cars), 2),
#                 c(mFormula(School_Codes ~ Dist | Licence + Cars + Income), 3),
#                 c(mFormula(School_Codes ~ Dist | Family + Level + Cars), 3),
#                 c(mFormula(School_Codes ~ Dist | Family + Level + Cars + Licence), 4),
#                 c(mFormula(School_Codes ~ Dist | Family + Level + Cars + Licence + Income), 5))

# ---- Prepare Output ----
num_segments = 7L
num_campuses = 7L
num_trials = 5L
num_metrics = 6L # Accuracy, Macro PRF, MCC, APO
metrics = array(numeric(), c(num_trials, num_metrics)) 
output = array(numeric(), c((num_segments + 4) * length(formulas), num_campuses + num_metrics + 1))

current_row = 3

for (call_idx in 1:length(formulas)) {
  print(call_idx[[1]])
  
  output[current_row-1,1:14] = c("ASC_MI", "ASC_OC", "ASC_RY", "ASC_SC", "ASC_SG", "ASC_YK",
                               "B_DIST", "Accuracy", "MacroPre", "MacroRec", "MacroF1",
                               "Matthews", "AvePrObs", "McF. R^2")
  
  # ---- Reference Model ----
  #model1 = mlogit(School_Codes ~ Dist, data=ml_data, reflevel="SG", subset = ml_data$Segment != 0, weights = ml_data$Exp_Segment)
  model1 = mlogit(School_Codes ~ Dist, data=ml_data, reflevel="SG")
  summary = summary(model1)
  print(summary)
  for (j in 1:num_campuses) {output[current_row, j] = model1[[1]][[j]]}
  x = fitted(model1, outcome = FALSE)
  print(x)
  write.table(x,'prediction_matrix.csv', sep=",", na="")
  for (j in 1:num_trials)
  {
    preds = vector()
    for (i in 1:nrow(x)) preds = c(preds, colnames(x)[sample(7, 1, prob = x[i,])])
    preds = as.factor(preds)
    y = confusionMatrix(preds, subset(df, Segment!=0)$School_Codes)
    metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]),
                    mcc(preds=preds, actuals=subset(df, Segment!=0)$School_Codes), mean(fitted(model1)))
  }
    output[current_row, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
  output[current_row,14] = summary[[20]][[1]]
  
  # ---- Segmented Model ----
  # for (i in 1:num_segments)
  # {
  #   print(i)
  #   model = mlogit(formulas[[call_idx]][[1]], data=ml_data, reflevel="YG", subset=ml_data$Segment==(i-1), weights = ml_data$Exp_Segment)
  #   summary = summary(model)
  #   # print(summary(model))
  #   #for (j in 1:num_campuses) output[current_row + i+1, j] = model[[1]][[j]]
  #   for (j in 1:num_campuses) output[current_row + i, j] = model[[1]][[j]]
  # 
  #   x = fitted(model, outcome = FALSE)
  #   for (j in 1:num_trials)
  #   {
  #     preds = vector()
  #     for (k in 1:nrow(x)) preds = c(preds, colnames(x)[sample(7, 1, prob = x[k,])])
  #     #for (i in 1:nrow(x)) preds = c(preds, colnames(x)[sample(7, 1, prob = x[i,])])
  #     preds = as.factor(preds)
  #     levels(preds) = levels(df$School_Codes)
  #     y = confusionMatrix(preds, subset(df, Segment == i-1)$School_Codes)
  #     y[[4]][,5][is.na(y[[4]][,5])] = 0
  #     y[[4]][,7][is.na(y[[4]][,7])] = 0
  # 
  #     metrics[j,] = c(y[[3]][1], mean(y[[4]][,5]), mean(y[[4]][,6]), mean(y[[4]][,7]),
  #                     mcc(preds=preds, actuals=subset(df, Segment == i-1)$School_Codes),
  #                     mean(fitted(model)))
  #   }
  #   output[current_row + i, (num_campuses+1):(num_campuses+num_metrics)] = apply(metrics, 2, mean)
  #   output[current_row + i, 14] = summary[[20]][[1]]
  # }
  # 
  # current_row = current_row + 11

}  
  
# ---- Print Output ----
#print(output)
#write.table(output, "Next_Steps_Results.csv", sep=",", na="")

#predict(model1, newdata = homes_df, se.fit = FALSE)
