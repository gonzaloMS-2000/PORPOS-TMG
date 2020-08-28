# Install packages ----
library(mlogit)
library(data.table)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Mode_Choice/Basic_Model")
source("../../../Metrics.R")

# Load Data ----
df = read.csv("../../../Data/SMTO_2015/Formatted.csv")
df = df[!(df$Time.Auto > 1000),]
df = df[!(df$Mode == "Other"),]
actuals = df$Mode = as.factor(df$Mode)
real_levels = levels(actuals)

# Mode Choice Data ----
df = df[c('Mode', 'Time.Auto', 'Time.Transit', 'Time.Active')]
mldf = mlogit.data(df, varying = 2:4, choice = "Mode", shape = "wide")

# Mode Choice Model ----
model = mlogit(Mode ~ 0 | 1 | Time, data=mldf, reflevel="Transit")
summary(model)

# Metrics ----
probs = fitted(model, outcome=FALSE)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
print_cm(hard_cm)
softmax_cm(probs, actuals, real_levels)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(model)
get_log_lik(model)