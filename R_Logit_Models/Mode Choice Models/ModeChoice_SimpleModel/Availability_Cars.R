# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
#df <- read.csv("MChInput_2015_withColumns_2.4.csv")

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

avail_type <- function(type_num, walk_thresh) {
  
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
  model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
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

## Write resutls to .csv file:
results = array(numeric(), c(12,4))
for (i in 1:12) {
  data = avail_type(i, 37.7)
  results[i,1] = data[1]
  results[i,2] = data[2]
  results[i,3] = data[3]
  results[i,4] = data[4]
}

d = as.data.frame(results)
setnames(d, old = c("V1", "V2", "V3", "V4"), new = c('Filtered Prob.','Total Prob.', 'Biased R^2', 'Filtered Sample Size'))
write.csv(d, "Avail_Outputs_46.6.csv", row.names = c(
   "All Available (Full Sample)", "Walk", "Car_Avail", "Car_Own (1+)", "Car_Own (2+)", "Car_Avail + Car_Own (1+)", "Car_Avail + Car_Own (2+)","Walk + Car_Avail", "Walk + Car_Own (1+)", "Walk + Car_Own (2+)", "Walk + Car_Avail + Car_Own (1+)", "Walk + Car_Avail + Car_Own (2+)"))



# # Write results to .csv file:
# coefs_and_stats = array(numeric(), c(8,4))
# x = summary(model)
# for (i in 1:5)
# {
#   coefs_and_stats[i,1] = x[[18]][[i]]
#   coefs_and_stats[i,2] = x[[18]][[i+15]]
#   coefs_and_stats[i,3] = x[[18]][[i+10]]
#   coefs_and_stats[i,4] = x[[18]][[i+5]]
# }
# 
# coefs_and_stats[7,1] = x[[2]][[1]]
# coefs_and_stats[8,1] = x[[20]][[1]]
# 
# 
# d = as.data.frame(coefs_and_stats)
# setnames(d, old = c('V1','V2', 'V3', 'V4'), new = c("Estimate","p-value","z-value","Std. Error"))
# write.csv(d, "ModeChoice_noCost_output.csv", row.names = c(
#   "ASC_Active", "ASC_Transit", "B_Auto", "B_Active", "B_Transit", "","LOG_LHOOD", "MCF_R^2"))



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
