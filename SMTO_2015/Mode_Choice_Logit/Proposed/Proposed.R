# Install packages ----
library(mlogit)
library(data.table)
setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/SMTO_2015/Mode_Choice_Logit/Proposed")
source("../../../Metrics.R")

# Load Data ----
df = read.csv("../../../Data/SMTO_2015/Formatted.csv")
df = df[!(df$Time.Auto > 1000),]
df = df[!(df$Mode == "Other"),]
actuals = df$Mode = as.factor(df$Mode)
real_levels = levels(actuals)

# Mode Choice Data ----
df = df[c('Mode', 'Licence', 'Family', 'Cars', 'Time.Auto', 'Time.Transit', 'Time.Active')]
mode_df <- df[!((df$Mode == "Active") & df$Time.Active >= 46.6),]
mode_df <- mode_df[!((mode_df$Mode == "Auto") & mode_df$Cars <= 1),]
mc_mldf = mlogit.data(mode_df, varying = 5:7, choice = "Mode", shape = "wide")
mc_mldf$available = (mc_mldf$alt == "Auto" & mc_mldf$Cars >= 2) | (mc_mldf$alt == "Transit") | (mc_mldf$alt == "Active" & mc_mldf$Time < 46.6)

# Mode Choice Model ----
model = mlogit(Mode ~ 0 | Family + Licence | Time, data=mc_mldf, reflevel="Transit", subset=mc_mldf$available)
coefs = coef(model)

# Probabilities ----
get_utility = function(alt, f, l, t, c)
{
  if (alt == "Transit") return(coefs[7] * t)
  else if (alt == "Auto")
  {
    if(c <= 1) return (-Inf)
    else return(coefs[2] + coefs[4] * f + coefs[6] * l + coefs[9] * t)
  } # Active
  else if(t > 46.6) return(-Inf)
  else return(coefs[1] + coefs[3] * f + coefs[5] * l + coefs[8] * t)
}

mldf = mlogit.data(df, varying = 5:7, choice = "Mode", shape = "wide")
mldf$Util = exp(mapply(get_utility, mldf$alt, mldf$Family, mldf$Licence, mldf$Time, mldf$Cars))
logsums = rep(colSums(matrix(mldf$Util, nrow=3)), each=3)
mldf$Prob = mldf$Util / logsums

# Metrics ----
probs = as.data.frame(t(matrix(mldf$Prob, nrow=3)))
names(probs) = c("Active", "Auto", "Transit")
colMeans(probs)
hard_preds = hardmax_preds(probs, real_levels)
hard_cm = get_cm(hard_preds, actuals)
t(print_cm(hard_cm))
softmax_cm(probs, actuals, real_levels)
get_prec(hard_cm)
get_rec(hard_cm)
get_f1(hard_cm)
get_accuracy(hard_cm)
get_softmax_accuracy(model)
get_log_lik(model)

softmax_probs = vector(length=length(actuals))
for (i in 1:length(actuals)) softmax_probs[i] = probs[i, which(colnames(probs)==actuals[i])]
mean(softmax_probs)
sum(log(softmax_probs))
