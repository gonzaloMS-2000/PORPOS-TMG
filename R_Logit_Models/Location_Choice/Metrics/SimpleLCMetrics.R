library(data.table)
library(mlogit)
library(caret)
library(e1071)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Simple")

df <- read.csv("../Campus_Choice_Segments.csv")
df$Campus = as.factor(df$Campus)

ml_data = mlogit.data(df, varying=2:8, choice="Campus", shape="wide")

model = mlogit(Campus ~ Dist, data=ml_data, reflevel="0")
summary(model)

preds = vector()
x = fitted(model, outcome = FALSE)
for (i in 1:nrow(x))
{
  preds = c(preds, sample(7, 1, prob = x[i,]) - 1)
}
head(preds)
preds = as.factor(preds)
  
