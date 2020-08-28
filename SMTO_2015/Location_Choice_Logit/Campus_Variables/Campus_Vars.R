# Imports ----
library(mlogit)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Location_Choice_Logit/Campus_Variables")
source("../../../Metrics.R")

# Load Data ----
df = read.csv("../../../Data/SMTO_2015/Formatted.csv")
df = df[!(df$Time.Auto > 1000),]

# Mode Choice Data ----
mode_df = df[c('Mode', 'Licence', 'Family', 'Cars', 'Time.Auto', 'Time.Transit', 'Time.Active')]
mode_df$Mode = as.factor(mode_df$Mode)
mode_df <- mode_df[!((mode_df$Mode == "Active") & mode_df$Time.Active >= 46.6),]
mode_df <- mode_df[!((mode_df$Mode == "Auto") & mode_df$Cars <= 1),]
mc_mldf = mlogit.data(mode_df, varying = 5:7, choice = "Mode", shape = "wide")
mc_mldf$available = (mc_mldf$alt == "Auto" & mc_mldf$Cars >= 2) | (mc_mldf$alt == "Transit") | (mc_mldf$alt == "Active" & mc_mldf$Time < 46.6)

# Mode Choice Model ----
mc_model = mlogit(Mode ~ 0 | Family + Licence | Time, data=mc_mldf, reflevel="Transit", subset=mc_mldf$available)
coefs = coef(mc_model)
Util.Transit = function(t) coefs[7] * t
Util.Active = function(f, l, t)
{
  if (t >= 46.6) return(-Inf)
  else return(coefs[1] + coefs[3] * f + coefs[5] * l + coefs[8] * t)
}
Util.Auto = function(f, l, t, c)
{
  if(c <= 1) return (-Inf)
  else return(coefs[2] + coefs[4] * f + coefs[6] * l + coefs[9] * t)
}

# Accessibility Logsums ----
actuals = df$School = as.factor(df$School)
real_levels = levels(actuals)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 19:95)
mldf$Transit = sapply(mldf$TPTT, Util.Transit)
mldf$Active = mapply(Util.Active, mldf$Family, mldf$Licence, mldf$Dist * 15)
mldf$Auto = mapply(Util.Auto, mldf$Family, mldf$Licence, mldf$AIVTT, mldf$Cars)
mldf$Access = log(apply(exp(mldf[, c("Transit", "Active", "Auto")]), 1, sum))

# Distance Model ----
model = mlogit(School ~ Dist + log(Total) + log(Tuition) + Admission + Domestic_UG | 0, data=mldf)
summary(model)
probs = get_probs(model)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_accuracy(hard_cm)
get_softmax_accuracy(model)
get_log_lik(model)

# Accessibility Model ----
model = mlogit(School ~ Access + log(Total) + log(Tuition) + Admission + Domestic_UG | 0, data=mldf)
summary(model)
probs = get_probs(model)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_accuracy(hard_cm)
get_softmax_accuracy(model)
get_log_lik(model)

# Combined Model ----
model = mlogit(School ~ Dist + Access + log(Total) + log(Tuition) + Admission + Domestic_UG | 0, data=mldf)
summary(model)
probs = get_probs(model)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
get_accuracy(hard_cm)
get_softmax_accuracy(model)
get_log_lik(model)