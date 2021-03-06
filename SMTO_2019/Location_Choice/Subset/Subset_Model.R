# Imports ----
library(mlogit)
library(data.table)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2019/Location_Choice/Subset")
source("../../../Metrics.R")

# Load and Format Data ----
df <- read.csv("../../../Data/SMTO_2019/Formatted.csv")
df = subset(df, School %in% c("SG", "SC", "MI", "YK", "YG", "RY", "OC"))
actuals = df$School = as.factor(droplevels(as.factor(df$School)))
real_levels = levels(actuals)
df$NonFamily = ifelse(df$Family == "False", 1, 0)
df$Family = ifelse(df$Family == "True", 1, 0)
df = df[, c(6, 13, 100, 38:44, 65:71, 93:99)]
mldf = mlogit.data(df, choice="School", shape="wide", varying=4:24)
mldf$Closest = as.integer(mldf$Closest & (mldf$Dist <= 2))

# Reference Model
ref = mlogit(School ~ Dist + log(Enrol) + Closest | 0, data=mldf)
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

# Family Model
fam = mlogit(School ~ Dist + log(Enrol) + Closest + Dist:Family | 0, data=mldf)
summary(fam)
probs = get_probs(fam)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(fam)
get_log_lik(fam)

# Non-Family Model
nf = mlogit(School ~ Dist + log(Enrol) + Closest + Dist:NonFamily | 0, data=mldf)
summary(nf)
probs = get_probs(nf)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(nf)
get_log_lik(nf)