# ---- Install packages ----
# require(devtools)
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df = subset(df, Segment!=0 & Family==1)
df$Time.Active = df$WTT
df$Time.Auto = df$AIVTT
df$Time.Transit = df$TPTT

mode_df = df[c(15, 6, 11, 93:95)]
mode_df <- mode_df[!(mode_df$Time.Auto > 1000),]
n = nrow(mode_df)


temp <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 45),]
temp <- temp[!((temp$Mode == "Auto") & temp$Cars <= 3),]
mldf = mlogit.data(temp, choice="Mode", shape="wide", varying = 4:6)
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 3) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 45)

# Run model
model = mlogit(Mode ~ 0 | Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
summary(model)
mean(fitted(model))
mean(fitted(model)) * nrow(mode_df) / n
