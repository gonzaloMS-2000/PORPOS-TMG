# Imports ----
library(mlogit)
library(data.table)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
source("../../Metrics.R")

# Load Data ----
df <- read.csv("../../Data/SMTO_2019/Formatted.csv")
rf_df = read.csv("../../2019 UniCollege Models/RF_Predictions.csv", row.names=1)
df = cbind(df, rf_df)
df$School = as.factor(df$School)
df$Family = ifelse(df$Family == "True", 1, 0)

# University Model ----
uni_df = subset(df, School_Type == 'University')
uni_df$School = droplevels(uni_df$School)
unis = levels(uni_df$School)
uni_cols = c(0, 1, 5, 6, 10:16)
uni_df = uni_df[, c(6, 13, 28 + uni_cols, 55 + uni_cols, 83 + uni_cols)]
uni_mldf = mlogit.data(uni_df, choice="School", shape="wide", varying=3:35)
uni_model = mlogit(School ~ Dist + log(Enrol) + Closest | 0, data=uni_mldf)
summary(uni_model)

# University Predictions ----
pred_uni_df = subset(df, Pred == 'Uni')
pred_uni_df = pred_uni_df[, c(6, 13, 28 + uni_cols, 55 + uni_cols, 83 + uni_cols)]
actuals = pred_uni_df$School
pred_uni_df$School = rep('SG', nrow(pred_uni_df))
levels(pred_uni_df$School) = unis
pred_uni_mldf = mlogit.data(pred_uni_df, choice="School", shape="wide", varying=3:35)
uni_probs = predict(uni_model, newdata=pred_uni_mldf)

# University Hardmax Metrics ----
hard_preds = hardmax_preds(uni_probs, colnames(uni_probs))
hard_cm = get_cm(hard_preds, actuals)
write.table(print_cm(hard_cm))
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)

# University Softmax Metrics ----
l = length(levels(df$School))
cm = matrix(data=NA, nrow=l, ncol=ncol(uni_probs))
for (i in 1:l){
  probs = uni_probs[which(actuals == levels(df$School)[i]),]
  if (is.null(nrow(probs))) cm[i,] = probs
  else cm[i,] = colSums(uni_probs[which(actuals == levels(df$School)[i]),])
}
write.table(cm, row.names = levels(df$School), col.names = colnames(uni_probs))

get_prob = function(s, probs){
  if (s %in% names(probs)) return(probs[as.character(s)])
  else return(0)
}
get_prob(actuals[511], probs[511,])

softmax_probs = vector(length=length(actuals))
for (i in 1:length(actuals))
  softmax_probs[i] = get_prob(actuals[i], uni_probs[i,])
mean(softmax_probs)
log(prod(softmax_probs))


# College Model ----
col_df = subset(df, School_Type == 'College')
col_df$School = droplevels(col_df$School)
cols = levels(col_df$School)
col_cols = c(0:9, 12:14, 17:19)
col_df = col_df[, c(6, 13, 18 + col_cols, 45 + col_cols, 73 + col_cols)]
col_mldf = mlogit.data(col_df, choice="School", shape="wide", varying = 3:50)
col_model = mlogit(School ~ Dist + log(Enrol) + Closest | 0, data=col_mldf)
summary(col_model)

# College Predictions ----
pred_col_df = subset(df, Pred == 'Col')
pred_col_df = pred_col_df[, c(6, 13, 18 + col_cols, 45 + col_cols, 73 + col_cols)]
actuals = pred_col_df$School
pred_col_df$School = rep('DOS', nrow(pred_col_df))
levels(pred_col_df$School) = cols
pred_col_mldf = mlogit.data(pred_col_df, choice="School", shape="wide", varying=3:50)
col_probs = predict(col_model, newdata=pred_col_mldf)

# College Hardmax Metrics ----
hard_preds = hardmax_preds(col_probs, colnames(col_probs))
hard_cm = get_cm(hard_preds, actuals)
write.table(print_cm(hard_cm))
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)

# College Softmax Metrics ----
l = length(levels(df$School))
cm = matrix(data=NA, nrow=l, ncol=ncol(col_probs))
for (i in 1:l){
  probs = col_probs[which(actuals == levels(df$School)[i]),]
  if (is.null(nrow(probs))) cm[i,] = probs
  else cm[i,] = colSums(col_probs[which(actuals == levels(df$School)[i]),])
}
write.table(cm, row.names = levels(df$School), col.names = colnames(col_probs))

get_prob = function(s, probs){
  if (s %in% names(probs)) return(probs[as.character(s)])
  else return(0)
}

softmax_probs = vector(length=length(actuals))
for (i in 1:length(actuals))
  softmax_probs[i] = get_prob(actuals[i], uni_probs[i,])
mean(softmax_probs)
log(prod(softmax_probs))

