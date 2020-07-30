# Packages ----
library(mlogit)
library(caret)
library(mltools)
library(data.table)


# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
df <- read.csv("../../Data/SMTO_2019/SMTO_2019_Complete_Input.csv")
df$Liv_Arr = as.factor(df$Liv_Arr)
df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Work = as.factor(df$Work)
df$Licence = as.logical.factor(df$Liv_Arr)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))
cols = setdiff(levels(df$School), unis)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 17:70)

# Reference (Unnested) Model
model = mlogit(School ~ Dist + Enrol | 0, data=mldf, reflevel='SG')
coef(model)
mean(fitted(model))

# Nested Model
nest_model = mlogit(School ~ Dist + Enrol | 0, data=mldf, reflevel='SG', nests=list(uni=unis, col=cols))
coef(nest_model)
mean(fitted(nest_model))

# Nested Model - Unique Elasticities
one_iv = update(nest_model, un.nest.el=TRUE)
coef(one_iv)
mean(fitted(one_iv))

# Nested Model - Unscaled
unscaled = update(nest_model, unscaled=TRUE)
coef(unscaled)
mean(fitted(unscaled))

lrtest(model, one_iv)
lrtest(one_iv, nest_model)
lrtest(unscaled, nest_model)

iv = coef(nest_model)['iv:uni'] # 1 - iv is correlation in unobserved factors within nest
var = vcov(nest_model)['iv:uni', 'iv:uni']
(iv - 1) / sqrt(var) # t-test for that iv = 1