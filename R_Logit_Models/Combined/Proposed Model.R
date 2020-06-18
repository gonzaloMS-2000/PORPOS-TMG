# ---- Install packages ----
# require(devtools)
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

# ---- Mode choice model ----
# Load data
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Combined")
mode_df <- read.csv("../Mode Choice Models/MChInput_2015_withColumns_2.4.csv")
mode_df = mode_df[c('Mode', 'Licence', 'Family', 'Cars', 'Time.Auto', 'Time.Transit', 'Time.Active')]

# Reformat data
mode_df$Mode = as.factor(mode_df$Mode)
mode_df$Licence = as.factor(mode_df$Licence)
mode_df$Family = (mode_df$Family == 'Family') * 1
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


# ---- Accessibility ----
campus_df <- read.csv("Combined_Input.csv")

# Reformat data
campus_df$Campus = as.factor(campus_df$Campus)
campus_df$Segment = as.factor(campus_df$Segment)
campus_df$Licence = as.factor(campus_df$Licence)
campus_df$Family = as.factor(campus_df$Family)

# Expansion factors
campus_df$Exp = ifelse(campus_df$Segment == 1, campus_df$Exp_SMTO, campus_df$Exp_Level)

for (i in 0:6)
{
  print(i)
  temp_df = campus_df[, c(2, 4, 5, (i*4)+7, (i*4)+8, (i*4)+10)]
  names(temp_df)[names(temp_df)==paste0("Time.Auto.", i)] <- "Time.Auto"
  names(temp_df)[names(temp_df)==paste0("Time.Transit.", i)] <- "Time.Transit"
  names(temp_df)[names(temp_df)==paste0("Time.Active.", i)] <- "Time.Active"
  temp_df$Mode = "Transit"
  temp_mldf = mlogit.data(temp_df, varying = 4:6, choice = "Mode", shape = "wide")
  access = as.data.frame(logsum(model, data=temp_mldf))
  names(access) <- paste0("Access.", i)
  campus_df = cbind(campus_df, access)

}

# ---- Location choice ----

# df2 = mldf2 to df format
# loc_mldf = mlogit.data(df2, varying = Campuses)
# model = ... (weights)
