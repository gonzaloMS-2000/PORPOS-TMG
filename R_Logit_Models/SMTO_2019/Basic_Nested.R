# Packages ----
library(mlogit)
library(data.table)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/SMTO_2019")
df <- read.csv("../../Data/SMTO_2019/SMTO_2019_Complete_Input.csv")
df$Liv_Arr = as.factor(df$Liv_Arr)
df$School = as.factor(df$School)
df$School_Type = as.factor(df$School_Type)
df$Work = as.factor(df$Work)
df$Licence = as.logical.factor(df$Liv_Arr)
df$Family = ifelse(df$Family, 1, 0)
unis = levels(droplevels(subset(df, School_Type == 'University')$School))
cols = setdiff(levels(df$School), unis)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 17:70)

# Reference (Unnested) Model
model = mlogit(School ~ Dist + Enrol + Dist:Family | 0, data=mldf, reflevel='SG')
coef(model)
mean(fitted(model))
probs = fitted(model, outcome=FALSE)

# Nested Model - Unique Elasticities
one_iv = mlogit(School ~ Dist + Enrol + Dist:Family | 0, data=mldf, reflevel='SG', nests=list(uni=unis, col=cols), un.nest.el=TRUE)
coef(one_iv)
mean(fitted(one_iv))
lrtest(model, one_iv)

# Nested Model
nest_model = update(one_iv, un.nest.el=FALSE)
coef(nest_model)
mean(fitted(nest_model))
lrtest(one_iv, nest_model)

iv = coef(nest_model)['iv:uni'] # 1 - iv is correlation in unobserved factors within nest
var = vcov(nest_model)['iv:uni', 'iv:uni']
(iv - 1) / sqrt(var) # t-test that iv = 1
iv = coef(nest_model)['iv:col'] # 1 - iv is correlation in unobserved factors within nest
var = vcov(nest_model)['iv:col', 'iv:col']
(iv - 1) / sqrt(var) # t-test that iv = 1

# codes = names(as.data.frame(probs))
# num_campuses = length(codes)
# cm = matrix(NA, nrow = num_campuses, ncol = num_campuses)
# for (i in 1:num_campuses){
#   cm[i, 1:num_campuses] = colSums(probs[which(df$School == codes[i]), ])
# }
# apo = sum(diag(cm)) / sum(cm)


