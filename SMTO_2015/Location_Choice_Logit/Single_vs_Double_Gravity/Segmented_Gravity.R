# Imports ----
library(mlogit)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015")
source("../Metrics.R")

# Load and Format Data ----
df <- read.csv("../Data/SMTO_2015/Formatted.csv")
df$School = as.factor(df$School)
df$Segment = (df$Level == 'Grad') * 4 + (df$Status == 'PT') * 2 + df$Family
df$Segment = ifelse(df$Segment == 7, 6, df$Segment)
table(df$Segment)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 19:95)
for (i in 0:6)
{
  cat("Segment:", i)
  model = mlogit(School ~ Dist | 1, data=mldf, subset=mldf$Segment==i, reflevel="SG")
  print(summary(model))
}

model = mlogit(School ~ Dist | 1, data=mldf, reflevel="SG")
print(summary(model))
