# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Active_Modifications/Inputs")
df <- read.csv("Input_withoutBikes.csv")

# Reformat data
df$Mode = as.factor(df$Mode)
df$Income = as.factor(df$Income)
df$Status = as.factor(df$Status)
df$Gender = as.factor(df$Gender)
df$Licence = as.factor(df$Licence)
df$Car_Avail = as.factor(df$Car_Avail)
df$Work = as.factor(df$Work)
df$Family = as.factor(df$Family)
df$Level = as.factor(df$Level)

## Remove outliers
# df_mode <- df[!(df$Time.Active > 600),]
# df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
# df_mode <- df_mode[!(df_mode$Time.Auto > 150),]
df <- df[!(df$Time.Auto > 1e3),]

# Filter unavailable choices
df_mode <- df[!((df$Mode == "Active") & df$Time.Active >= 36.4),]
#df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
#df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]


# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")

# Availability Column
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 36.4)
model = mlogit(Mode ~ 0|1|Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
b = summary(model)

# Prepare output dataframe
out = data.frame("Call" = paste(toString(b[[17]][[2]][[3]][2]), " | ", toString(b[[17]][[2]][[3]][3])),
                 "Total Accuracy" = mean(fitted(model)) * nrow(df_mode) / nrow(df),
                 "Null" = b[[20]][[1]], "Time" = 0, "Adj." = 0)

# Formulas
formulas = list(c(mFormula(Mode ~ 0|Family|Time), 1),
                c(mFormula(Mode ~ 0|Children|Time), 1),
                c(mFormula(Mode ~ 0|Adults|Time), 1),
                c(mFormula(Mode ~ 0|Income|Time), 1),
                c(mFormula(Mode ~ 0|Family + Children|Time), 2),
                c(mFormula(Mode ~ 0|Family + Adults|Time), 2),
                c(mFormula(Mode ~ 0|Adults + Children|Time), 2),
                c(mFormula(Mode ~ 0|Family + Income|Time), 2),
                c(mFormula(Mode ~ 0|Family + Children + Adults|Time), 3),
                c(mFormula(Mode ~ 0|Family + Children + Income|Time), 3),
                c(mFormula(Mode ~ 0|Family + Adults + Income|Time), 3),
                c(mFormula(Mode ~ 0|Family + Children + Adults + Income|Time), 4),
                c(mFormula(Mode ~ 0|Licence|Time), 1),
                c(mFormula(Mode ~ 0|Income + Licence|Time), 2),
                c(mFormula(Mode ~ 0|Family + Licence|Time), 2),
                c(mFormula(Mode ~ 0|Family + Income + Licence|Time), 3),
                c(mFormula(Mode ~ 0|Family + Children + Adults + Income + Licence|Time), 5),
                c(mFormula(Mode ~ 0|Gender|Time), 1),
                c(mFormula(Mode ~ 0|Level|Time), 1),
                c(mFormula(Mode ~ 0|Status|Time), 1),
                c(mFormula(Mode ~ 0|Work|Time), 1),
                c(mFormula(Mode ~ 0|Gender + Level|Time), 2),
                c(mFormula(Mode ~ 0|Gender + Status|Time), 2),
                c(mFormula(Mode ~ 0|Gender + Work|Time), 2),
                c(mFormula(Mode ~ 0|Level + Work|Time), 2),
                c(mFormula(Mode ~ 0|Status + Work|Time), 2),
                c(mFormula(Mode ~ 0|Gender + Level + Work|Time), 3),
                c(mFormula(Mode ~ 0|Gender + Status + Work|Time), 3),
                c(mFormula(Mode ~ 0|Level + Family|Time), 2),
                c(mFormula(Mode ~ 0|Level + Income|Time), 2),
                c(mFormula(Mode ~ 0|Level + Family + Income|Time), 3),
                c(mFormula(Mode ~ 0|Work + Income|Time), 2),
                c(mFormula(Mode ~ 0|Work + Level + Income|Time), 3))

for (call in formulas) {
  print(call[[1]])
  
  # Run Model
  model = mlogit(call[[1]], data = mldf, reflevel = "Transit", subset = mldf$available == 1)
  s = summary(model)
  out[nrow(out) + 1,] = c(paste(toString(s[[17]][[2]][[3]][2]), " | ", toString(s[[17]][[2]][[3]][3])),
                          mean(fitted(model)) * nrow(df_mode) / nrow(df),
                          s[[20]][1],
                          1 - s[[2]][[1]]/b[[2]][[1]],
                          1 - (s[[2]][[1]] - call[[2]])/b[[2]][[1]])
}

out
# write.csv(d, "../Active_Modifications/ChSpVars_withoutBikes.csv", row.names = vars)
# 
# # Write results to file
# write.csv(out, "Outputs_Avai+ChSpVars_withBikeTimes.csv")
# 
# #../Active_Modifications/
