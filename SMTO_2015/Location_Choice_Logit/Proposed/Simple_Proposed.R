# Imports ----
library(mlogit)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Location_Choice_Logit/Proposed")
source("../../../Metrics.R")

# Load and Format Data ----
df <- read.csv("../../../Data/SMTO_2015/Formatted.csv")
df$School = as.factor(df$School)
actuals = df$School
real_levels = levels(actuals)

# Reorder closest columns
codes = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
for (code in codes) df[[paste0('IsClosest.', code)]] = as.integer((df[[paste0('Dist.', code)]] <= 2) & df[[paste0('Closest.', code)]])

# Convert to long format
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(19:95, 100:106))

# Run Model ----
model = mlogit(School ~ Dist + log(Total) + Family:Dist + IsClosest | 0, data=mldf)
summary(model)

# Metrics ----
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