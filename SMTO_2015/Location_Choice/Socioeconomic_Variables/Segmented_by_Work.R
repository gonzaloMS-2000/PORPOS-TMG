# Packages ----
library(mlogit)

# Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Socioeconomic_Variables")
df <- read.csv("../../Data/SMTO_2015/Formatted.csv")
df$School = as.factor(df$School)

codes = c("SG", "SC", "MI", "YK", "YG", "RY", "OC")
for (code in codes) df[[paste0('IsClosest.', code)]] = as.integer((df[[paste0('Dist.', code)]] <= 2) & df[[paste0('Closest.', code)]])

work_status = function(s)
{
  if (startsWith(s, "Yes, I work p")) return("NA")
  else if (startsWith(s, "Yes")) return("FT")
  else return("NA")
}
df$Work = relevel(as.factor(sapply(df$Work, work_status)), "NA")

mldf = mlogit.data(df, choice="School", shape="wide", varying = c(19:95, 100:106))

# Models ----
model = mlogit(School ~ Dist + log(Total) + Family:Dist + IsClosest | 0, data=mldf)
summary(model)
mean(fitted(model))

m2 = mlogit(School ~ Dist + log(Total) + Family:Dist + IsClosest + Work:Dist | 0, data=mldf)
summary(m2)
mean(fitted(m2))

m3 = mlogit(School ~ Dist + log(Total) + Family:Dist + IsClosest | 0, data=mldf, subset = mldf$Work=='FT')
summary(m3)
mean(fitted(m3)) # 228

m4 = mlogit(School ~ Dist + log(Total) + Family:Dist + IsClosest | 0, data=mldf, subset = mldf$Work!='FT')
summary(m4)
mean(fitted(m4)) # 14250

(mean(fitted(m3)) * 228 + mean(fitted(m4)) * 14250) / (228 + 14250)
mean(fitted(model))
mean(fitted(m2))
