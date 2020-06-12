library(data.table)
library(mlogit)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Enrollment")

df <- read.csv("../R_Segmented_Input.csv")
df$Campus = as.factor(df$Campus)

num_intercepts = 2L
num_segments = 7L
coefs_and_stats = array(numeric(), c(num_intercepts + 4, num_segments))

for (i in 0:(num_segments-1))
#for (i in 1:1)
{
  ml_data = mlogit.data(df, varying=4:80, choice="Campus", shape="wide",
                        subset = Segment == i)
  
  if (i == 0)
  {
    model = mlogit(Campus ~ Dist + Total | 0, data=ml_data, reflevel="YG")
    model2 = mlogit(Campus ~ get(paste("S", i, sep="")) | 0, data=ml_data, reflevel="YG")
    # model3 = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG")
    model4 = mlogit(Campus ~ Total | 0, data=ml_data, reflevel="YG")
  }
  else if (i < 4) 
  {
    model = mlogit(Campus ~ Dist + UG | 0, data=ml_data, reflevel="YG")
    model2 = mlogit(Campus ~ get(paste("S", i, sep="")) | 0, data=ml_data, reflevel="YG")
    #model3 = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG")
    model4 = mlogit(Campus ~ UG | 0, data=ml_data, reflevel="YG")
  }
  else
  {
    model = mlogit(Campus ~ Dist + Grad | 0, data=ml_data, reflevel="YG")
    model2 = mlogit(Campus ~ get(paste("S", i, sep="")) | 0, data=ml_data, reflevel="YG")
    # model3 = mlogit(Campus ~ Dist, data=ml_data, reflevel="YG")
    model4 = mlogit(Campus ~ Grad | 0, data=ml_data, reflevel="YG")
  }
  
  for (j in 1:num_intercepts)
  {
    coefs_and_stats[j, i+1] = model[[1]][[j]]
  }
  
  coefs_and_stats[num_intercepts+1, i+1] = model[[2]][[1]] # Log likelihood - distance and enrollment
  coefs_and_stats[num_intercepts+2, i+1] = model2[[2]][[1]] # Log likelihood - enrollment only
  coefs_and_stats[num_intercepts+3, i+1] = model4[[2]][[1]] # Log likelihood - distance only
  # coefs_and_stats[num_intercepts+4, i+1] = model3[[2]][[1]] # Log likelihood - gravity
  coefs_and_stats[num_intercepts+4, i+1] = 1 - (model[[2]][[1]]) / (model2[[2]][[1]]) # Rho squared - enrollment
  # coefs_and_stats[num_intercepts+6, i+1] = 1 - (model[[2]][[1]]) / (model4[[2]][[1]]) # Rho squared - distance
}

d = as.data.frame(coefs_and_stats)
setnames(d, old = c('V1','V2', 'V3', 'V4', 'V5', 'V6', 'V7'), new = c("0", "1", "2", "3", "4", "5", "6"))
write.csv(d, "Segmented_Output_Enrol.csv", row.names = c("B_DIST", "B_ENROL", "D_ENR_LHOOD", "REF_LHOOD", "ENR_LHOOD", "R^2"))
# write.csv(d, "Segmented_Output_Enrol.csv", row.names = c("B_DIST", "B_ENROL", "D_ENR_LHOOD", "ENR_LHOOD", "D_LHOOD", "GRV_LHOOD", "ENR_R^2", "D_R^2"))

