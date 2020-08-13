library(mlogit)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG")
source("Metrics.R")

# Sample Usage ----
df <- read.csv("Data/SMTO_2015/Formatted.csv")
df$School = as.factor(df$School)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 19:95)
model = mlogit(School ~ Dist | 0, data=mldf)
actuals = df$School # Observations
real_levels = levels(actuals)

# Function Calls ----
probs = get_probs(model)
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