# ---- Packages ----
library(mlogit)
library(data.table)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Nested")
df <- read.csv("../../Data/SMTO_2015/Formatted.csv")
df = subset(df, Segment!=0)
df$School = as.factor(df$School)
codes = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
for (code in codes) df[[paste0('IsClosest.', code)]] = as.integer((df[[paste0('Dist.', code)]] <= 2) & df[[paste0('Closest.', code)]])
df = df[c(18, 10, 19:25, 40:46, 100:106, 99)]
mldf = mlogit.data(df, choice="School", shape="wide", varying = c(3:23))

# Proposed Model (No Closest)
model1 = mlogit(School ~ Dist + Total + Family:Dist | 0, data=mldf, weights=mldf$Exp_Segment)
summary(model1)[[18]]
mean(fitted(model1))

# Proposed Model (Closest <2km)
model2 = mlogit(School ~ Dist + Total + Family:Dist + IsClosest | 0, data=mldf, weights=mldf$Exp_Segment)
summary(model2)[[18]]
mean(fitted(model2))

# Nested Model (Unique, No Closest)
model3 = mlogit(School ~ Dist + Total + Family:Dist | 0, data=mldf, weights=mldf$Exp_Segment,
                nests=list(dt = c("SG", "RY", "OC"), other = c("SC", "MI", "YG", "YK")), un.nest.el=TRUE)
summary(model3)[[18]]
mean(fitted(model3))
lrtest(model1, model3)

# Nested Model (Unique, Closest)
model4 = mlogit(School ~ Dist + Total + Family:Dist + IsClosest | 0, data=mldf, weights=mldf$Exp_Segment,
                nests=list(dt = c("SG", "RY", "OC"), other = c("SC", "MI", "YG", "YK")), un.nest.el=TRUE)
summary(model4)[[18]]
mean(fitted(model4))
lrtest(model2, model4)

# Nested Model (No Closest)
model5 = mlogit(School ~ Dist + Total + Family:Dist | 0, data=mldf, weights=mldf$Exp_Segment,
               nests=list(dt = c("SG", "RY", "OC"), other = c("SC", "MI", "YG", "YK")))
summary(model5)[[18]]
mean(fitted(model5))
lrtest(model3, model5)

# Nested Model (Closest)
model6 = mlogit(School ~ Dist + Total + Family:Dist + IsClosest | 0, data=mldf, weights=mldf$Exp_Segment,
                nests=list(dt = c("SG", "RY", "OC"), other = c("SC", "MI", "YG", "YK")))
summary(model6)[[18]]
mean(fitted(model6))
lrtest(model4, model6)
