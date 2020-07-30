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
mldf = mlogit.data(df, choice="School", shape="wide", varying = 19:45)

# Reference (Unnested) Model
model = mlogit(School ~  Dist, data=mldf, reflevel = 'SG')
summary(model)
mean(fitted(model))

# Nested Model
nest_model = mlogit(School ~  Dist, data=mldf, reflevel = 'SG', nests = list(uni = unis, col = cols))
summary(nest_model)
mean(fitted(nest_model))

# Nested Model - Unique Elasticities
nest_model_unique = mlogit(School ~  Dist, data=mldf, reflevel = 'SG', nests = list(uni = unis, col = cols), un.nest.el=TRUE)
summary(nest_model_unique)
mean(fitted(nest_model_unique))

# Nested Model - Unscaled
nest_model_unscaled = mlogit(School ~  Dist, data=mldf, reflevel = 'SG', nests = list(uni = unis, col = cols), unscaled=TRUE)
summary(nest_model_unscaled)
mean(fitted(nest_model_unscaled))