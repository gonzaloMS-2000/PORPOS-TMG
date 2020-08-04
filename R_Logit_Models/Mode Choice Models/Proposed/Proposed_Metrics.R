# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Proposed")
df <- read.csv("Input_withoutRoommates.csv")

# Data formatting
df$Mode = as.factor(df$Mode)
df$Family = ifelse(df$Family == 'Family', 1, 0)
df <- df[!(df$Time.Auto > 1000),] # Remove outliers
total_n = nrow(df)

# Filter unavailable choices
filter_df <- df[!((df$Mode == "Active") & df$Time.Active >= 46.6),]
filter_df <- filter_df[!((filter_df$Mode == "Auto") & filter_df$Cars <= 1),]

# Transform dataframe from wide to long
mldf = mlogit.data(filter_df, varying = 13:15, choice = "Mode", shape = "wide")
mldf$available = (mldf$alt == "Auto" & mldf$Cars >= 2) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 46.6)

# Run model
model = mlogit(Mode ~ 0 | Licence + Family | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
summary(model)

# Results
paste0("In-Sample Accuracy: ", mean(fitted(model)))
paste0("Total Accuracy: ", mean(fitted(model)) * nrow(df_mode) / total_n)

new_mldf = mlogit.data(df, varying = 13:15, choice = "Mode", shape = "wide")
new_mldf$available = (new_mldf$alt == "Auto" & new_mldf$Cars >= 2) | (new_mldf$alt == "Transit") | (new_mldf$alt == "Active" & new_mldf$Time < 46.6)

# Run model
full_model = as.data.frame(predict(model, new_mldf))
alts = names(full_model)
for (i in 1:length(alts)){
  cm[i,] = colSums(full_model[which(df$Mode == alts[i]),])
}
cm

write.table(cm, col.names = alts, row.names = alts)
