library(mlogit)
library(data.table)

# Read ModeChoice_input.csv
df <- read.csv("MChInput_2015_withColumns_2.3.csv")
df$Mode = as.factor(df$Mode)
df$Income = as.factor(df$Income)
df$Status = as.factor(df$Status)
df$Gender = as.factor(df$Gender)
df$Licence = as.factor(df$Licence)
df$Work = as.factor(df$Work)
df$Family = as.factor(df$Family)
df$Level = as.factor(df$Level)

#str(df)

df_mode <- df[!(df$Time.Active > 600),]
df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
df_mode <- df_mode[!(df_mode$Time.Auto > 150),]

# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
#head(mldf)

base = mlogit(Mode ~ 0|1|Time, data = mldf, reflevel = "Auto")
b = summary(base)
model = mlogit(Mode ~ 0|Family|Time, data = mldf, reflevel = "Auto")
s = summary(model)

k = 1
cat("R^2 Rel. Null: ", s[[20]][[1]], "\nR^2 Rel. Time: ",  1 - (s[[2]][[1]])/b[[2]][[1]], "\nR^2 Adj. Time: ", 1 - (s[[2]][[1]] - k)/b[[2]][[1]])


writeLines(paste("R^2 Rel. Null,", "R^2 Rel. Time,", "R^2 Adj. Time", sep="\n"), con = "OutputsModeChoice.csv")
writeLines(paste(toString(s[[20]][[1]]), ",", toString(1 - (s[[2]][[1]])/b[[2]][[1]]), ",", toString(1 - (s[[2]][[1]] - k)/b[[2]][[1]]), sep = "\n"), con = "OutputsModeChoice.csv")

R1 = c()
R2 = c()
R3 = c()



df_rho = data.fram("R^2 Rel. Null", "R^2 Rel. Time", "R^2 Adj. Time")
