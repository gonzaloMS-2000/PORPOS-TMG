library(data.table)
library(mlogit)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice")

df <- read.csv("Segment_Exp_Input.csv")
df$Campus = as.factor(df$Campus)



num_segments = 7L
# coefs_and_stats = array(numeric(), c(9, num_segments))

for (i in 0:(num_segments-1))
{
  ml_data = mlogit.data(df, varying=4:31, choice="Campus", shape="wide", subset=Segment==i)
  
  ref = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG")
  smt = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG", weights=ml_data$Exp_SMTO)
  tot = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG", weights=ml_data$Exp_Total)
  lev = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG", weights=ml_data$Exp_Level)

  # coefs_and_stats[1, i+1] = ref[[2]][[1]] # Log likelihood
  # coefs_and_stats[2, i+1] = enr[[2]][[1]] # Log likelihood
  # coefs_and_stats[3, i+1] = dst[[2]][[1]] # Log likelihood
  # coefs_and_stats[4, i+1] = end[[2]][[1]] # Log likelihood
  # coefs_and_stats[5, i+1] = grv[[2]][[1]] # Log likelihood
  # 
  # coefs_and_stats[6, i+1] = 1 - (enr[[2]][[1]])/(ref[[2]][[1]]) # Rho squared
  # coefs_and_stats[7, i+1] = 1 - (dst[[2]][[1]])/(ref[[2]][[1]]) # Rho squared
  # coefs_and_stats[8, i+1] = 1 - (end[[2]][[1]])/(ref[[2]][[1]]) # Rho squared
  # coefs_and_stats[9, i+1] = 1 - (grv[[2]][[1]])/(ref[[2]][[1]]) # Rho squared
}

# d = as.data.frame(coefs_and_stats)
# setnames(d, old = c('V1','V2', 'V3', 'V4', 'V5', 'V6', 'V7'), new = c("0", "1", "2", "3", "4", "5", "6"))
# write.csv(d, "Comparison_Output.csv", row.names = c("REF_LLH", "ENR_LLH", "DST_LLH", "E+D_LLH", "GRV_LLH", "ENR_R^2", "DST_R^2", "E+D_R^2", "GRV_R^2"))


