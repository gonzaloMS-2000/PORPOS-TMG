# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Active_Modifications")
df <- read.csv("Input_Final_Filtered.csv")

table(df$Mode)

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
df_mode <- df[!(df$Time.Auto > 1e3),]

# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")

# Run reference model
model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit")
out = data.frame("Threshold" = -1,
                 "R^2" = summary(model)[[20]][1],
                 "In-Sample Accuracy" = mean(fitted(model)),
                 "Full Sample Accuracy" = mean(fitted(model)))

# for (t in c(seq(15, 23, by=2), seq(25, 37, by=0.2), seq(40, 60, by=5), seq(75, 120, by=15), seq(180, 300, by=60)))
# for (t in c(seq(25, 35, by=1)))
# for (t in c(seq(15, 39, by=2), seq(40, 50, by=0.2), seq(51, 55, by=1), seq(60, 120, by=15), seq(180, 300, by=60)))
# {
#   print(t)
#   df_mode2 <- df[!(df$Time.Auto > 1e3),]
#   df_mode2 <- df_mode2[!((df_mode$Mode == "Active") & df_mode$Time.Active >= t),]
#   mldf2 = mlogit.data(df_mode2, varying = 13:15, choice = "Mode", shape = "wide")
#   mldf2$available = (mldf2$alt == "Auto") | (mldf2$alt == "Transit") | (mldf2$alt == "Active" & mldf2$Time < t)
#   model = mlogit(Mode ~ 0| 1 | Time, data = mldf2, reflevel = "Transit", subset = mldf2$available)
#   
#   probs = as.data.frame(predict(model, newdata = mldf))
#   df_mode$prob.tr = probs$Transit
#   df_mode$prob.ac = probs$Active
#   df_mode$prob.au = probs$Auto
#   
#   out[nrow(out)+1,] = c(t, summary(model)[[20]][1], mean(fitted(model)),
#                         mean(ifelse(df_mode$Mode == "Transit", df_mode$prob.tr,
#                              ifelse(df_mode$Mode == "Active", df_mode$prob.ac,
#                                     df_mode$prob.au))))
#   # print(cat("Threshold = ", t, "\tR^2: ", summary(model)[[20]][[1]]))
# }

# for (t in c(seq(15, 23, by=2), seq(25, 37, by=0.2), seq(40, 60, by=5), seq(75, 120, by=15), seq(180, 300, by=60)))
# for (t in c(seq(40, 60, by=5)))
for (t in c(seq(10, 20, by=1), seq(20, 50, by=0.2), seq(50, 70, by=1), seq(70, 180, by=10), seq(180, 300, by=60)))
{
  print(t)
  df_mode2 <- df[!(df$Time.Auto > 1e3),]
  df_mode2 <- df_mode2[!((df_mode$Mode == "Active") & df_mode$Time.Active >= t),]
  mldf2 = mlogit.data(df_mode2, varying = 13:15, choice = "Mode", shape = "wide")
  mldf2$available = (mldf2$alt == "Auto") | (mldf2$alt == "Transit") | (mldf2$alt == "Active" & mldf2$Time < t)
  model = mlogit(Mode ~ 0| 1 | Time, data = mldf2, reflevel = "Transit", subset = mldf2$available)
  
  out[nrow(out)+1,] = c(t, summary(model)[[20]][1], mean(fitted(model)),
                        nrow(df_mode2) * mean(fitted(model)) / nrow(df_mode))
  # print(nrow(df_mode2))
  # print(cat("Threshold = ", t, "\tAccuracy: ", nrow(df_mode2) * mean(fitted(model)) / nrow(df_mode)))
}

write.csv(out, "FILTERED_Active_Threshold.csv")
