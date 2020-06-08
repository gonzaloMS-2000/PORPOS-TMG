library(data.table)
library(mlogit)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice")

df <- read.csv("Campus_Choice_Segments_Enrol.csv")
df$Campus = as.factor(df$Campus)

num_intercepts = 2L
num_segments = 7L
coefs_and_stats = array(numeric(), c(num_intercepts + 1, num_segments))
log_likelihoods = vector("numeric", num_segments)
rho_squareds = vector("numeric", num_segments)
for (i in 0:(num_segments-1))
{
  ml_data = mlogit.data(df, varying=3:30, choice="Campus", shape="wide",
                        subset = Segment == i)

  if (i == 0) model = mlogit(Campus ~ Dist + Total | 0, data=ml_data, reflevel="5")
  else if (i < 4) model = mlogit(Campus ~ Dist + UG | 0, data=ml_data, reflevel="5")
  else model = mlogit(Campus ~ Dist + Grad | 0, data=ml_data, reflevel="5")
  
  model2 = mlogit(Campus ~ Dist, data=ml_data, reflevel="5")
  
  coefs_and_stats[num_intercepts+1, i+1] = model[[2]][[1]] # Log likelihood
  print(1 - (model[[2]][[1]]) / (model2[[2]][[1]]))
  for (j in 1:num_intercepts)
  {
    coefs_and_stats[j, i+1] = model[[1]][[j]]
  }

}

d = as.data.frame(coefs_and_stats)
setnames(d, old = c('V1','V2', 'V3', 'V4', 'V5', 'V6', 'V7'), new = c("0", "1", "2", "3", "4", "5", "6"))
write.csv(d, "Segmented_Output_Enrol.csv", row.names = c("B_DIST", "B_ENROL", "LOG_LHOOD"))