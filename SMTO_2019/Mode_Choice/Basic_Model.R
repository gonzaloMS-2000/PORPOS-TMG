library(mlogit)
library(data.table)

# Read ModeChoice_input.csv
df <- read.csv("C:/Users/gonza/Desktop/ModeChoice_2019Input_withoutCost.csv")
df$Mode = as.factor(df$Mode)
str(df)

dim(df)

# # # Plot original densities
d <- density(df_mode$Time.Auto)
plot(d, main="Density of Transit Times")
polygon(d, col="red", border="blue")

df_mode <- df[!(df$Time.Active > 600),]
df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
df_mode <- df_mode[!(df_mode$Time.Auto > 150),]

# Transform dataframe from wide to long
mldf = mlogit.data(df_mode, varying = 2:4, choice = "Mode", shape = "wide")
head(mldf)

# Run MNL model
model = mlogit(Mode ~ 0|1|Time, data = mldf, reflevel = "Auto")
summary(model)
x = summary(model)

# Write results to .csv file:
coefs_and_stats = array(numeric(), c(8,4))
x = summary(model)
for (i in 1:5)
{
  coefs_and_stats[i,1] = x[[18]][[i]]
  coefs_and_stats[i,2] = x[[18]][[i+15]]
  coefs_and_stats[i,3] = x[[18]][[i+10]]
  coefs_and_stats[i,4] = x[[18]][[i+5]]
}

coefs_and_stats[7,1] = x[[2]][[1]]
coefs_and_stats[8,1] = x[[20]][[1]]


d = as.data.frame(coefs_and_stats)
setnames(d, old = c('V1','V2', 'V3', 'V4'), new = c("Estimate","p-value","z-value","Std. Error"))
write.csv(d, "ModeChoice_noCost_output_2019.csv", row.names = c(
  "ASC_Active", "ASC_Transit", "B_Auto", "B_Active", "B_Transit", "","LOG_LHOOD", "MCF_R^2"))