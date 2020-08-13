# Install packages
#require(devtools) 
#install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Cost")
df<- read.csv("Mode_Choice_Cost_Input.csv")

#df$Cost.Active = runif(nrow(df), 0, 1)
df$Cost.Active = (sample(101, size = nrow(df), replace = TRUE) - 1)
#df$Cost.Active = 0

# Reformat data
df$Mode = relevel(as.factor(df$Mode), "Auto")
df$Licence = as.factor(df$Licence)
df$Family = as.factor(df$Family)

# Filter unavailable choices
df_mode <- df[!((df$Mode == "Active") & df$Time.Active >= 46.6),]
df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
df_mode <- df_mode[!(df_mode$Time.Active >= 46.6 & df_mode$Cars <= 1),]

# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 6:11, choice = "Mode", shape = "wide")
# head(mldf, 12)

# Column for availability
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 46.6)
# table(mldf$available)

# Run MNL model
model = mlogit(Mode ~ Cost | 1 | Time, data = mldf, reflevel = "Active", subset = mldf$available)
# model = mlogit(Mode ~ Cost | 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
print(summary(model))

total_n = nrow(df_mode)
print(cat("Total Accuracy: ", mean(fitted(model)) * nrow(df_mode) / total_n))