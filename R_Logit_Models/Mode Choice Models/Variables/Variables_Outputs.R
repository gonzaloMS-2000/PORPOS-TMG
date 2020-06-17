# Import packages
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Variables")
df <- read.csv("../MChInput_2015_withColumns_2.3.csv")

# Reformat data
df$Mode = as.factor(df$Mode)
df$Income = as.factor(df$Income)
df$Status = as.factor(df$Status)
df$Gender = as.factor(df$Gender)
df$Licence = as.factor(df$Licence)
df$Work = as.factor(df$Work)
df$Family = as.factor(df$Family)
df$Level = as.factor(df$Level)

# Remove outliers
df_mode <- df[!(df$Time.Active > 600),]
df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
df_mode <- df_mode[!(df_mode$Time.Auto > 150),]

# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")

# Run reference model
b = summary(mlogit(Mode ~ 0|1|Time, data = mldf, reflevel = "Auto"))

# Prepare output dataframe
out = data.frame("Call" = paste(toString(b[[17]][[2]][[3]][2]), " | ", toString(b[[17]][[2]][[3]][3])), "Null" = b[[20]][[1]], "Time" = 0, "Adj." = 0)

# List of calls and k
calls = list(# c(mFormula(Mode ~ 0|Income|Time), 1),
             # c(mFormula(Mode ~ 0|Children|Time), 1),
             # c(mFormula(Mode ~ 0|Adults|Time), 1),
             # c(mFormula(Mode ~ 0|Family|Time), 1),
             # c(mFormula(Mode ~ 0|Family + Children|Time), 2),
             # c(mFormula(Mode ~ 0|Family + Adults|Time), 2),
             # c(mFormula(Mode ~ 0|Adults + Children|Time), 2),
             # c(mFormula(Mode ~ 0|Family + Income|Time), 2), 
             # c(mFormula(Mode ~ 0|Family + Children + Adults|Time), 3),
             # c(mFormula(Mode ~ 0|Family + Children + Income|Time), 3),
             # c(mFormula(Mode ~ 0|Family + Adults + Income|Time), 3),
             # c(mFormula(Mode ~ 0|Family + Children + Adults + Income|Time), 4)
             # c(mFormula(Mode ~ 0|Cars|Time), 1),
             # c(mFormula(Mode ~ 0|Licence|Time), 1),
             # c(mFormula(Mode ~ 0|Car_Avail|Time), 1),
             # c(mFormula(Mode ~ 0|Cars + Licence|Time), 2),
             # c(mFormula(Mode ~ 0|Cars + Car_Avail|Time), 2),
             # c(mFormula(Mode ~ 0|Cars + Income|Time), 2),
             # c(mFormula(Mode ~ 0|Income + Licence|Time), 2),
             # c(mFormula(Mode ~ 0|Income + Car_Avail|Time), 2),
             # c(mFormula(Mode ~ 0|Car_Avail + Licence|Time), 2),
             # c(mFormula(Mode ~ 0|Cars + Car_Avail + Licence|Time), 3),
             # c(mFormula(Mode ~ 0|Cars + Income + Licence|Time), 3),
             # c(mFormula(Mode ~ 0|Cars + Car_Avail + Income|Time), 3),
             # c(mFormula(Mode ~ 0|Income + Car_Avail + Licence|Time), 3),
             # c(mFormula(Mode ~ 0|Cars + Car_Avail + Licence + Income|Time), 4)
             # c(mFormula(Mode ~ 0|Family|Time), 1),
             # c(mFormula(Mode ~ 0|Cars|Time), 1),
             # c(mFormula(Mode ~ 0|Family + Cars|Time), 2),
             # c(mFormula(Mode ~ 0|Family + Children + Cars|Time), 3),
             # c(mFormula(Mode ~ 0|Family + Cars + Licence|Time), 3),
             # c(mFormula(Mode ~ 0|Family + Cars + Income|Time), 3),
             # c(mFormula(Mode ~ 0|Family + Children + Cars + Licence|Time), 4),
             # c(mFormula(Mode ~ 0|Family + Children + Cars + Licence + Income|Time), 5),
             # c(mFormula(Mode ~ 0|Family + Children + Adults + Cars|Time), 4),
             # c(mFormula(Mode ~ 0|Family + Children + Adults + Cars + Income|Time), 5),
             # c(mFormula(Mode ~ 0|Family + Cars + Licence + Car_Avail|Time), 4),
             # c(mFormula(Mode ~ 0|Family + Cars + Licence + Car_Avail + Income|Time), 5),
             # c(mFormula(Mode ~ 0|Family + Children + Cars + Licence + Car_Avail + Income|Time), 6),
             # c(mFormula(Mode ~ 0|Family + Children + Adults + Cars + Licence + Car_Avail + Income|Time), 7)
             # c(mFormula(Mode ~ 0|Gender|Time), 1),
             # c(mFormula(Mode ~ 0|Level|Time), 1),
             # c(mFormula(Mode ~ 0|Status|Time), 1),
             # c(mFormula(Mode ~ 0|Work|Time), 1),
             # c(mFormula(Mode ~ 0|Gender + Level|Time), 2),
             # c(mFormula(Mode ~ 0|Gender + Status|Time), 2),
             # c(mFormula(Mode ~ 0|Gender + Work|Time), 2),
             # # c(mFormula(Mode ~ 0|Level + Status|Time), 2), # Singular
             # c(mFormula(Mode ~ 0|Level + Work|Time), 2),
             # c(mFormula(Mode ~ 0|Status + Work|Time), 2),
             # # c(mFormula(Mode ~ 0|Gender + Level + Status|Time), 3),
             # c(mFormula(Mode ~ 0|Gender + Level + Work|Time), 3),
             # c(mFormula(Mode ~ 0|Gender + Status + Work|Time), 3)
             # # c(mFormula(Mode ~ 0|Status + Level + Work|Time), 3)
             # # c(mFormula(Mode ~ 0|Gender + Status + Level + Work|Time), 4)
             c(mFormula(Mode ~ 0|Family|Time), 1),
             c(mFormula(Mode ~ 0|Cars|Time), 1),
             c(mFormula(Mode ~ 0|Level|Time), 1),
             c(mFormula(Mode ~ 0|Level + Family|Time), 2),
             c(mFormula(Mode ~ 0|Level + Cars|Time), 2),
             c(mFormula(Mode ~ 0|Level + Family + Cars|Time), 3)
             )

# Run model
for (call in calls)
{
  print(call[[1]])
  s = summary(mlogit(call[[1]], data = mldf, reflevel = "Auto"))
  out[nrow(out) + 1,] = c(paste(toString(s[[17]][[2]][[3]][2]), " | ", toString(s[[17]][[2]][[3]][3])), s[[20]][1], 1 - s[[2]][[1]]/b[[2]][[1]], 1 - (s[[2]][[1]] - call[[2]])/b[[2]][[1]])
}

# Write results to file
write.csv(out, "Level_Outputs.csv")