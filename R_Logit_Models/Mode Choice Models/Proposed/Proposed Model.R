# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Proposed")
df <- read.csv("Input_withoutRoommates.csv")

# Reformat data
df$Mode = as.factor(df$Mode)
df$Income = as.factor(df$Income)
df$Status = as.factor(df$Status)
df$Gender = as.factor(df$Gender)
df$Licence = as.factor(df$Licence)
df$Work = as.factor(df$Work)
df$Family = as.factor(df$Family)
df$Level = as.factor(df$Level)

table(df$Mode)

# Remove outliers
df_mode <- df[!(df$Time.Auto > 1000),]
total_n = nrow(df_mode)

# Filter unavailable choices
df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= 46.6),]
df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]

# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")

# Availability column
mldf$available = (mldf$alt == "Auto" & mldf$Cars >= 2) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 46.6)

# Run model
model = mlogit(Mode ~ 0 | 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
summary(model)

# Results
print(cat("In-Sample Accuracy: ", mean(fitted(model))))
print(cat("Total Accuracy: ", mean(fitted(model)) * nrow(df_mode) / total_n))