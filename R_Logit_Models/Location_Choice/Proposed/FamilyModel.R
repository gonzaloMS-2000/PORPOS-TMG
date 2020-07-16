# ---- Install packages ----
require(devtools)
install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)
library(caret)
library(e1071)
library(mltools)

# ---- Load and Format Data ----
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df = subset(df, Segment!=0 & Family==1)
df$School_Codes = as.factor(df$School_Codes)
df$Work = as.factor(df$Work)
df$Work = relevel(df$Work, "Unknown")
df$Status = as.factor(df$Status)
df$Status = relevel(df$Status, "FT")
df$Income = as.factor(df$Income)
df$Income = relevel(df$Income, "Unknown")
df$Mode = as.factor(df$Mode)

mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))
mldf$TPTT = mldf$TPTT/60
mldf$AIVTT = mldf$AIVTT/60

# ---- Variables ----
num_campuses = 7L
actuals = df$School_Codes
vars = c("Work", "Status", "Income")
for (var in vars) {
  formula = paste0("School_Codes ~ Dist + Enrol + Dist:", var, " | 0")
  model = mlogit(as.formula(formula), data=mldf, weights=mldf$Exp_Segment, reflevel="SG")
  probs = fitted(model, outcome = FALSE)
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / num_campuses
  f1 = sum(y[[4]][,7], na.rm=TRUE) / num_campuses
  metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
  names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
  cat(var, "\n")
  print(metrics)
  print(summary(model))
}


model = mlogit(School_Codes ~ Dist + Enrol + Licence:AIVTT | 0, data=mldf, weights=mldf$Exp_Segment, reflevel="SG")
# ---- One Run ----
probs = fitted(model, outcome = FALSE)
preds = vector()
for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
preds = as.factor(preds)
levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
y =  suppressWarnings(confusionMatrix(preds, actuals))
prec = sum(y[[4]][,5], na.rm=TRUE) / num_campuses
f1 = sum(y[[4]][,7], na.rm=TRUE) / num_campuses
metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)))
names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
summary(model)
metrics

# ---- Mode choice model ----
mode_df = df[c('Level', 'Status', 'Licence', 'Work', )]
mode_df = mlogit.data(df, choice="Mode", shape="wide", varying = 18:87)
mode_df$Time.Auto = mode_df$AIVTT
mode_df$Time.Active = mode_df$WTT
mode_df$Time.Transit = mode_df$TPTT

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
# campus_df <- read.csv("Combined_Input.csv")
campus_df <- read.csv("Combined_Input_Access.csv")

# Reformat data
campus_df$Campus = as.factor(campus_df$Campus)
campus_df$Segment = as.factor(campus_df$Segment)
campus_df$Licence = as.factor(campus_df$Licence)
campus_df$Family = as.factor(campus_df$Family)

# for (i in 0:6)
# {
#   print(i)
#   temp_df = campus_df[, c(2:4, (i*4)+6, (i*4)+7, (i*4)+9)]
#   names(temp_df)[names(temp_df)==paste0("Time.Auto.", i)] <- "Time.Auto"
#   names(temp_df)[names(temp_df)==paste0("Time.Transit.", i)] <- "Time.Transit"
#   names(temp_df)[names(temp_df)==paste0("Time.Active.", i)] <- "Time.Active"
#   temp_df$Mode = "Transit"
#   temp_mldf = mlogit.data(temp_df, varying = 4:6, choice = "Mode", shape = "wide")
#   mldf_av = mldf[mldf$available]
#   access = as.data.frame(logsum(model, data=temp_mldf))
#   names(access) <- paste0("Access.", i)
#   campus_df = cbind(campus_df, access)
# }

# ---- Location choice ----

# Convert data to long format
campus_df = campus_df[c(1, 5, seq(8, 32, by=4), 34:41)]
lc_mldf = mlogit.data(campus_df, varying = c(3:9, 11:17), choice = "Campus", shape = "wide")

# Prepare output matrix
num_segments = 7L
num_ascs = 6L
coefs_and_stats = array(numeric(), c(num_ascs + 4, num_segments*6))

# Iterate over segments
for (i in 0:6)
{
  print(i)
  
  lc_model_1 = mlogit(Campus ~ Dist, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i)
  lc_model_2 = mlogit(Campus ~ Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i)
  lc_model_3 = mlogit(Campus ~ Dist + Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i)
  lc_model_4 = mlogit(Campus ~ Dist, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i, weights = lc_mldf$Exp)
  lc_model_5 = mlogit(Campus ~ Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i, weights = lc_mldf$Exp)
  lc_model_6 = mlogit(Campus ~ Dist + Access, data=lc_mldf, reflevel="4", subset = lc_mldf$Segment == i, weights = lc_mldf$Exp)
  for (k in 1:6)
  {
    col = (6*i)+k
    for (j in 1:num_ascs)
    {
      coefs_and_stats[j, col] = get(paste0("lc_model_", k))[[1]][[j]]
    }
    add = 1
    if (k %in% c(1, 3, 4, 6))  {
      coefs_and_stats[j+1, col] = get(paste0("lc_model_", k))[[1]][[j+add]]
      add = 2   }
    if (k %in% c(2, 3, 5, 6)) coefs_and_stats[j+2, col] = get(paste0("lc_model_", k))[[1]][[j+add]]
    
    coefs_and_stats[j+3, col] = get(paste0("lc_model_", k))[[2]][[1]]
    coefs_and_stats[j+4, col] = summary(get(paste0("lc_model_", k)))[[20]][[1]]
  }
}
coefs_and_stats
write.csv(coefs_and_stats, file="Combined_Output.csv", na = "0",
          row.names = c("ASC_SG", "ASC_SC", "ASC_MI", "ASC_YK", "ASC_RY", "ASC_OC",
                        "B_DIST", "B_ACCESS", "LLHOOD", "MCF R2"))



