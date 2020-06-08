library(data.table)
library(mlogit)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice")

df <- read.csv("Campus_Choice_Segments.csv")
df$Campus = as.factor(df$Campus)

num_intercepts = 7L
num_segments = 7L
coefs_and_stats = array(numeric(), c(num_intercepts + 2,num_segments))
log_likelihoods = vector("numeric", num_segments)
rho_squareds = vector("numeric", num_segments)
for (i in 0:(num_segments-1))
{
  ml_data = mlogit.data(df, varying=2:8, choice="Campus", shape="wide",
                        subset = Segment == i)
  
  model = mlogit(Campus ~ Dist, data=ml_data, reflevel="5")

  coefs_and_stats[num_intercepts+1, i+1] = model[[2]][[1]] # Log likelihood
  coefs_and_stats[num_intercepts+2, i+1] = summary(model)[[20]][[1]] # Rho squared
  for (j in 1:num_intercepts)
  {
    coefs_and_stats[j, i+1] = model[[1]][[j]]
  }
  
}

d = as.data.frame(coefs_and_stats)
setnames(d, old = c('V1','V2', 'V3', 'V4', 'V5', 'V6', 'V7'), new = c("0", "1", "2", "3", "4", "5", "6"))
write.csv(d, "Segmented_Output.csv", row.names = c(
  "ASC_SG", "ASC_SC", "ASC_MI", "ASC_YK", "ASC_RY", "ASC_OC",
  "B_DIST", "LOG_LHOOD", "MCF_R^2"))