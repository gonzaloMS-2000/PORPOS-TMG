# Install packages
# require(devtools)
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Combined")
mode_df <- read.csv("../Mode Choice Models/MChInput_2015_withColumns_2.4.csv")
mode_df = mode_df[c('Mode', 'Licence', 'Family', 'Cars', 'Time.Auto', 'Time.Transit', 'Time.Active')]

# Reformat data
mode_df$Mode = as.factor(mode_df$Mode)
mode_df$Licence = as.factor(mode_df$Licence)
mode_df$Family = as.factor(mode_df$Family)

# Remove outliers and unavailable choices
mode_df <- mode_df[!(mode_df$Time.Auto > 1000),]
mode_df <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 46.6),]
mode_df <- mode_df[!((mode_df$Mode == "Auto") & mode_df$Cars <= 1),]

# Transform dataframe from wide to long
mldf = mlogit.data(mode_df, varying = 5:7, choice = "Mode", shape = "wide")
mldf$available = (mldf$alt == "Auto" & mldf$Cars >= 2) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 46.6)

# Run model
model = mlogit(Mode ~ 0 | Family + Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
summary(model)

# predict(model, newdata = NULL, returnData = FALSE, ...)

# df = CampusChoiceDf.csv
# mldf2 = df in mldf format
# predict(mldf2, returnData = TRUE) - https://cran.r-project.org/web/packages/mlogit/mlogit.pdf
# df2 = mldf2 to df format
# loc_mldf = mlogit.data(df2, varying = Campuses)
# model = ... (weights)

# Licence, Family, Segment, Cars
# Dist, Time.Active, Time.Transit, Time.Auto for each campus
# Expansion factors
