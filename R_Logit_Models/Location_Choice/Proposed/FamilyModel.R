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

# ---- Mode Choice Data ----
mode_df = df[c(15, 6, 11, 90:92)]
mode_df = subset(mode_df, Mode!="Other" & Time.Auto < 1000)
actuals = as.factor(mode_df$Mode)
full_mldf = mlogit.data(mode_df, choice="Mode", shape="wide", varying = 4:6)

# ---- Metrics Calculator ----
get_metrics <- function(model) {
  probs = predict(model, newdata = full_mldf)
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / 3
  f1 = sum(y[[4]][,7], na.rm=TRUE) / 3
  metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)) * length(fitted(model)) / length(actuals))
  names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
  return(metrics)
}

# ---- Base Model ----
mldf = mlogit.data(mode_df, choice="Mode", shape="wide", varying = 4:6)
model = mlogit(Mode ~ 0 | Licence | Time, data = mldf, reflevel = "Transit")
print("Base Model")
get_metrics(model)

# ---- 0+ Cars ----
temp <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 45),]
mldf = mlogit.data(temp, choice="Mode", shape="wide", varying = 4:6)
mldf$available = (mldf$alt == "Auto") | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 45)
model = mlogit(Mode ~ 0 | Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
print("0+ Cars")
get_metrics(model)

# ---- 1+ Cars ----
temp <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 45),]
temp <- temp[!((temp$Mode == "Auto") & temp$Cars <= 0),]
mldf = mlogit.data(temp, choice="Mode", shape="wide", varying = 4:6)
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 0) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 45)
model = mlogit(Mode ~ 0 | Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
print("1+ Cars")
get_metrics(model)

# ---- 2+ Cars ----
temp <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 45),]
temp <- temp[!((temp$Mode == "Auto") & temp$Cars <= 1),]
mldf = mlogit.data(temp, choice="Mode", shape="wide", varying = 4:6)
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 45)
model = mlogit(Mode ~ 0 | Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
print("2+ Cars")
get_metrics(model)

# ---- 3+ Cars ----
temp <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 45),]
temp <- temp[!((temp$Mode == "Auto") & temp$Cars <= 2),]
mldf = mlogit.data(temp, choice="Mode", shape="wide", varying = 4:6)
mldf$available = (mldf$alt == "Auto" & mldf$Cars > 2) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 45)
model = mlogit(Mode ~ 0 | Licence | Time, data = mldf, reflevel = "Transit", subset = mldf$available)
print("3+ Cars")
get_metrics(model)

