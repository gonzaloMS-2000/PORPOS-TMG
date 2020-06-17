# Install packages
require(devtools) 
install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Availability")
df <- read.csv("MChInput_2015_withColumns_2.3.csv")

# Reformat data
df$Mode = relevel(as.factor(df$Mode), "Auto")
df$Income = as.factor(df$Income)
df$Status = as.factor(df$Status)
df$Gender = as.factor(df$Gender)
df$Licence = as.factor(df$Licence)
df$Car_Avail = as.factor(df$Car_Avail)
df$Work = as.factor(df$Work)
df$Family = as.factor(df$Family)
df$Level = as.factor(df$Level)

# Remove outliers
# df_mode <- df[!(df$Time.Active > 600),]
# df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
# df_mode <- df_mode[!(df_mode$Time.Auto > 150),]
df_mode <- df[!(df$Time.Auto > 1e3),]

# # Filter unavailable choices
# df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= 300),]
# # df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 0),]
# # df_mode <- df_mode[!(df_mode$Time.Active >= 300 & df_mode$Cars <= 0),]
# 
# # Transform dataframe from wide to long
# mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
# # head(mldf, 12)
# 
# # Column for availability
# # mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 300)
# # mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0) | (mldf$alt == "Transit") | (mldf$alt == "Active")
# mldf$available = (mldf$alt == "Auto") | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 300)
# table(mldf$available)
# 
# # Run MNL model
# model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
# print(summary(model))

model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit")

# out = data.frame("Threshold" = -1, "R^2" = summary(model)[[20]][1])
# for (t in c(seq(15, 23, by=2), seq(25, 37, by=0.2), seq(40, 60, by=5), seq(75, 120, by=15), seq(180, 300, by=60)))
# for (t in c(seq(25, 35, by=1)))
# {
#   print(t)
#   df_mode <- df[!(df$Time.Auto > 1e3),]
#   df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= t),]
#   mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
#   mldf$available = (mldf$alt == "Auto") | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < t)
#   model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
#   out[nrow(out)+1,] = c(t, summary(model)[[20]][1])
#   # print(cat("Threshold = ", t, "\tR^2: ", summary(model)[[20]][[1]]))
# }
# 
# write.csv(out, "Active_Thresholds.csv")
