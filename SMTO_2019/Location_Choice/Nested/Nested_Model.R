# Packages ----
library(mlogit)
library(data.table)
source('../../../Metrics.R')

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2019/Location_Choice/Nested")
df <- read.csv("../../../Data/SMTO_2019/Formatted.csv")
actuals = df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Family = ifelse(df$Family == "True", 1, 0)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))
cols = setdiff(levels(df$School), unis)
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(18:71, 73:99))
mldf$Closest = as.integer(mldf$Closest == 1 & mldf$Dist <= 2)

# Reference (Unnested) Model
model = mlogit(School ~ Dist + log(Enrol) + Dist:Family + Closest | 0, data=mldf)
summary(model)
hard_preds = hardmax_preds(fitted(model, outcome=FALSE), levels(actuals))
hard_cm = get_cm(hard_preds, actuals)
get_accuracy(hard_cm)
mean(fitted(model))
get_log_lik(model)

# Nested Model - Unique Elasticities
one_iv = mlogit(School ~ Dist + log(Enrol) + Dist:Family + Closest | 0, data=mldf,
                nests=list(uni=unis, col=cols), un.nest.el=TRUE)
summary(one_iv)
hard_preds = hardmax_preds(fitted(one_iv, outcome=FALSE), levels(actuals))
hard_cm = get_cm(hard_preds, actuals)
get_accuracy(hard_cm)
mean(fitted(one_iv))
get_log_lik(one_iv)

# Nested Model
nest_model = update(one_iv, un.nest.el=FALSE)
summary(nest_model)
hard_preds = hardmax_preds(fitted(nest_model, outcome=FALSE), levels(actuals))
hard_cm = get_cm(hard_preds, actuals)
get_accuracy(hard_cm)
mean(fitted(nest_model))
get_log_lik(nest_model)
