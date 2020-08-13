# Install package
library(mlogit)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Segmented")
df <- read.csv("../MChInput_2015_withColumns_2.4.csv")

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

# Segment column
df$Segment = ifelse(df$Level == "Other", 0,
                    ifelse(df$Level == "UG",
                           ifelse(df$Status == "PT", 3,
                                  ifelse(df$Family == "Family", 1, 2)),
                           ifelse(df$Status == "PT", 6,
                                  ifelse(df$Family == "Family", 4, 5))))

# Remove outliers
df <- df[!(df$Time.Auto > 1e3),]

# Transform data frame from wide to long
mldf = mlogit.data(df, varying = 13:15, choice = "Mode", shape = "wide")
head(mldf)

# Prepare output
num_intercepts = 5L
num_segments = 7L
coefs_and_stats = array(numeric(), c(num_intercepts + 4, num_segments + 1))

# Iterate through segments
for (i in -1:(num_segments-1))
{
  # Run model
  if (i == -1) model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit")
  else model = mlogit(Mode ~ 0| 1 | Time, data = mldf, reflevel = "Transit", subset = Segment == i)
  
  coefs_and_stats[1, i+2] = sum(model[[15]]) # Sample size
  coefs_and_stats[num_intercepts+2, i+2] = model[[2]][[1]] # Log likelihood
  coefs_and_stats[num_intercepts+3, i+2] = summary(model)[[20]][[1]] # Rho squared
  coefs_and_stats[num_intercepts+4, i+2] = mean(fitted(model)) # Model Fit
  for (j in 1:num_intercepts)
  {
    coefs_and_stats[j+1, i+2] = model[[1]][[j]]
  }
  
}

d = as.data.frame(coefs_and_stats)
setnames(d, old = c('V1','V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8'),
            new = c("Full", "0", "1", "2", "3", "4", "5", "6"))
write.csv(d, "Segmented_Output.csv", row.names = c("Sample Size",
  "ASC_AC", "ASC_AU", "B_TPTT", "B_ACTT", "B_AIVTT",
  "LLHOOD", "MCF_R2", "Fit"))
