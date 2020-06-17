# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Availability")
df_visualize <- read.csv("../MChInput_2015_withColumns_2.4.csv")

# Reformat data
# df$Mode = relevel(as.factor(df$Mode), "Auto")
# df$Income = as.factor(df$Income)
# df$Status = as.factor(df$Status)
# df$Gender = as.factor(df$Gender)
# df$Licence = as.factor(df$Licence)
# df$Car_Avail = as.factor(df$Car_Avail)
# df$Work = as.factor(df$Work)
# df$Family = as.factor(df$Family)
# df$Level = as.factor(df$Level)

# Remove outliers
# df_mode <- df[!(df$Time.Active > 600),]
# df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
# df_mode <- df_mode[!(df_mode$Time.Auto > 150),]
# df_mode <- df[!(df$Time.Auto > 1e3),]
# 
# df_mode_original = df_mode

# # Filter unavailable choices
# df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= 300),]
# df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
# df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
# df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
# 
# 
# # Transform dataframe from wide to long
# mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
# head(mldf, 12)


avail_func <- function(type_num, walk_thresh,ch_sp_var) {
  # This funciton only supports a max of 2 choice specific variables (ch_sp_var)
  # Write ch_sp_var = 1 for NO choice specific variables
  
  # Load data
  df <- read.csv("MChInput_2015_withColumns_2.4.csv")
  
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

  # Make copy of original mode
  df_mode_original = df_mode

  data = c()
  #headers = c("Cars, Licence")
  
  # Columns for availability
  if (type_num == 1) {
    # All Available (Full Sample)
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto") | (mldf$alt == "Transit") | (mldf$alt == "Active")
 
  } else if (type_num == 2) {
    # Walk
    df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= walk_thresh),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto") | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < walk_thresh)

  } else if (type_num == 3) {
    # Car_Avail
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Car_Avail == 1) | (mldf$alt == "Transit") | (mldf$alt == "Active")

  } else if (type_num == 4) {
    # Cars (1+)
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0) | (mldf$alt == "Transit") | (mldf$alt == "Active")

  } else if (type_num == 5) {
    # Cars (2+)
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1) | (mldf$alt == "Transit") | (mldf$alt == "Active")

  } else if (type_num == 6) {
    # Car_Avail + Cars (1+)
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0 & mldf$Car_Avail == 1) | (mldf$alt == "Transit") | (mldf$alt == "Active")

  } else if (type_num == 7) {
    # Car_Avail + Cars (2+)
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1 & mldf$Car_Avail == 1) | (mldf$alt == "Transit") | (mldf$alt == "Active")

    } else if (type_num == 8) {
    # Walk + Car_Avail
    df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= walk_thresh),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Car_Avail == 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < walk_thresh)

  } else if (type_num == 9) {
    # Walk + Cars (1+)
    df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= walk_thresh),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < walk_thresh)

  } else if (type_num == 10) {
    # Walk + Cars (2+)
    df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= walk_thresh),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < walk_thresh)

  } else if (type_num == 11) {
    # Walk + Car_Avail  + Cars (1+)
    df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= walk_thresh),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0 & mldf$Car_Avail == 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < walk_thresh)

  } else if (type_num == 12) {
    # Walk + Car_Avail + Cars (2+)
    df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= walk_thresh),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
    df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
    mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
    mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1 & mldf$Car_Avail == 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < walk_thresh)
  }
  
  # Run MNL model
  if (length(ch_sp_var) == 1) {
    model = mlogit(Mode ~ 0| get(ch_sp_var) | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
  }
  else {
    model = mlogit(Mode ~ 0| get(ch_sp_var[1]) + get(ch_sp_var[2]) | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
  }
  
  print(summary(model))
  x = summary(model)
  
  ## Probability Criteria
  # In-Sample
  data[1] = sum(mldf$Mode & mldf$available) * mean(fitted(model)) / nrow(df_mode_original)
  # Total
  data[2] = mean(fitted(model))
  # R^2
  data[3] = x[[20]][[1]]
  # Final Rows
  data[4] = nrow(df_mode)

  return(data)
}

# #results = array(numeric(), c(6,4))
# results1 = avail_func(4, 46.6, "Cars")
# results2 = avail_func(4, 46.6, "Licence")
# results3 = avail_func(4, 46.6, c("Cars", "Licence"))
# #results4 = avail_func(5, 46.6, "Cars")
# results5 = avail_func(5, 46.6, "Licence")
# #results6 = avail_func(5, 46.6, c("Cars", "Licence"))
# 
# results7 = avail_func(2, 46.6, "Cars")
# results8 = avail_func(2, 46.6, "Licence")

results9 = avail_func(9, 46.6, "Licence")
results10 = avail_func(10, 46.6, "Licence")

## Write resutls to .csv file:
results = array(numeric(), c(2,4))
# runs = c(results1, results2,results3,results5,results7,results8)
# 
# results[1,1] = results1[1]
# results[1,2] = results1[2]
# results[1,3] = results1[3]
# results[1,4] = results1[4]
# 
# results[2,1] = results2[1]
# results[2,2] = results2[2]
# results[2,3] = results2[3]
# results[2,4] = results2[4]
#   
# results[3,1] = results3[1]
# results[3,2] = results3[2]
# results[3,3] = results3[3]
# results[3,4] = results3[4]
#   
# results[5,1] = results5[1]
# results[5,2] = results5[2]
# results[5,3] = results5[3]
# results[5,4] = results5[4]
#   
# results[7,1] = results7[1]
# results[7,2] = results7[2]
# results[7,3] = results7[3]
# results[7,4] = results7[4]
# 
# results[8,1] = results8[1]
# results[8,2] = results8[2]
# results[8,3] = results8[3]
# results[8,4] = results8[4]

results[1,1] = results9[1]
results[1,2] = results9[2]
results[1,3] = results9[3]
results[1,4] = results9[4]

results[2,1] = results10[1]
results[2,2] = results10[2]
results[2,3] = results10[3]
results[2,4] = results10[4]

d = as.data.frame(results)
setnames(d, old = c("V1", "V2", "V3", "V4"), new = c('Total Prob.','In-Sample Prob.', 'Biased R^2', 'Filtered Sample Size'))
write.csv(d, "Avail_Outputs_chSpVar.csv")

# ## Write resutls to .csv file:
# results = array(numeric(), c(12,4))
# for (i in 1:12) {
#   data = avail_func(i, 37.7)
#   results[i,1] = data[1]
#   results[i,2] = data[2]
#   results[i,3] = data[3]
#   results[i,4] = data[4]
# }
# 
# d = as.data.frame(results)
# setnames(d, old = c("V1", "V2", "V3", "V4"), new = c('Total Prob.','In-Sample Prob.', 'Biased R^2', 'Filtered Sample Size'))
# write.csv(d, "Avail_Outputs_46.6.csv", row.names = c(
#    "All Available (Full Sample)", "Walk", "Car_Avail", "Car_Own (1+)", "Car_Own (2+)", "Car_Avail + Car_Own (1+)", "Car_Avail + Car_Own (2+)","Walk + Car_Avail", "Walk + Car_Own (1+)", "Walk + Car_Own (2+)", "Walk + Car_Avail + Car_Own (1+)", "Walk + Car_Avail + Car_Own (2+)"))








# model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit")
# 
# out = data.frame("Threshold" = -1, "R^2" = summary(model)[[20]][1])
# for (t in c(seq(15, 23, by=2), seq(25, 37, by=0.2), seq(40, 60, by=5), seq(75, 120, by=15), seq(180, 300, by=60)))
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
