# Imports ----
library(mlogit)
library(data.table)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_Combined")
source("../../Metrics.R")
library(car)

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
schools_2019 = setdiff(levels(df$School), schools_2015)

# Nested - Two Parameters
nl2 = mlogit(School ~ Dist + log(Enrol) + Closest + Dist:Family | 0, data=mldf,
            nests = list(smto_2015 = schools_2015, smto_2019 = levels(df$School)))
summary(nl2)
probs = get_probs(nl2)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(nl2)
get_log_lik(nl2)

# Nested - One Parameter
nl1 = update(nl2, un.nest.el=TRUE)
summary(nl1)
probs = get_probs(nl1)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(nl1)
get_log_lik(nl1)


ref = mlogit(School ~ Dist + log(Enrol) + Closest + Dist:Family | 0, data=mldf)
lrtest(ref, nl1)

linearHypothesis(nl2, "iv:smto_2015 = iv:smto_2019")
