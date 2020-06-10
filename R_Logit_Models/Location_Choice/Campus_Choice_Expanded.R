# Import packages
library(data.table)
library(mlogit)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice")
df <- read.csv("Segment_Exp_Input.csv")

# Reformat data
df$Campus = as.factor(df$Campus)

# Prepare output matrix
num_segments = 7L
num_intercepts = 7L
coefs_and_stats = array(numeric(), c(num_intercepts + 2, num_segments*4))

# Iterate over segments
for (i in 0:(num_segments-1))
{
  # Prepare data in long format
  ml_data = mlogit.data(df, varying=4:31, choice="Campus", shape="wide", subset=Segment==i)
  
  # Run models
  ref = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG")
  smt = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG", weights=ml_data$Exp_SMTO)
  tot = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG", weights=ml_data$Exp_Total)
  lev = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG", weights=ml_data$Exp_Level)

  # Write estimated parameters
  for (j in 1:num_intercepts)
  {
    coefs_and_stats[j, (4*i)+1] = ref[[1]][[j]]
    coefs_and_stats[j, (4*i)+2] = smt[[1]][[j]]
    coefs_and_stats[j, (4*i)+3] = tot[[1]][[j]]
    coefs_and_stats[j, (4*i)+4] = lev[[1]][[j]]
  }
  
  # Write log likelihoods
  coefs_and_stats[j+1, (4*i)+1] = ref[[2]][[1]]
  coefs_and_stats[j+1, (4*i)+2] = smt[[2]][[1]]
  coefs_and_stats[j+1, (4*i)+3] = tot[[2]][[1]]
  coefs_and_stats[j+1, (4*i)+4] = lev[[2]][[1]]
  
  # Write rho-squared
  coefs_and_stats[j+2, (4*i)+1] = summary(ref)[[20]][[1]]
  coefs_and_stats[j+2, (4*i)+2] = summary(smt)[[20]][[1]]
  coefs_and_stats[j+2, (4*i)+3] = summary(tot)[[20]][[1]]
  coefs_and_stats[j+2, (4*i)+4] = summary(lev)[[20]][[1]]
}

# Print output to file
d = as.data.frame(coefs_and_stats)
setnames(d, old = c('V1','V2', 'V3', 'V4'), new = c("Ref.", "SMTO", "By Total", "By Level"))
write.csv(d, "Exp_Comparison.csv", row.names = c("ASC_SG", "ASC_SC", "ASC_MI", "ASC_YK", "ASC_RY", "ASC_OC", "B_DIST", "L_LHOOD", "MCF_R^2"))

