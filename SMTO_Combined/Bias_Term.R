# Imports ----
library(mlogit)
library(data.table)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_Combined")
source("../../Metrics.R")

# Load and Format Data ----
df <- read.csv("../../Data/SMTO_Combined/Formatted.csv")
actuals = df$School = as.factor(df$School)
real_levels = levels(actuals)
df$NonFamily = ifelse(df$Family == "False", 1, 0)
df$Family = ifelse(df$Family == "True", 1, 0)
df = df[, c(1, 3, 7, 96, 15:41, 42:68, 69:95)]
mldf = mlogit.data(df, choice="School", shape="wide", varying=5:85)
mldf$Closest = as.integer((mldf$Closest == 'True') & (mldf$Dist <= 2))
schools_2015 = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
mldf$available = ifelse(mldf$Source == 2015, mldf$alt %in% schools_2015, TRUE)

# Availability Model ----
model = mlogit(School ~ Dist + log(Enrol) + Closest | 0, data=mldf, subset=mldf$available)
summary(model)

# model = mlogit(School ~ Dist | 0, data=mldf, subset=mldf$available)
summary(model)
probs = get_probs(model)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(fam)
get_log_lik(fam)

# Bias Model ----
ref = mlogit(School ~ Dist + log(Enrol) + Closest + log(Enrol):In_2015 | 0, data=mldf)
summary(ref)
probs = get_probs(ref)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(ref)
get_log_lik(ref)
