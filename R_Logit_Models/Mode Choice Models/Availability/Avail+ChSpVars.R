# Install packages
# require(devtools) 
# install_version("mlogit", version = "0.4-2", repos = "http://cran.us.r-project.org")
library(mlogit)
library(data.table)

setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Mode Choice Models/Availability")

run_model = function(call) {
  
  # Load data
  df <- read.csv("../MChInput_2015_withColumns_2.4.csv")
  
  # Reformat data
  df$Mode = as.factor(df$Mode)
  df$Income = as.factor(df$Income)
  df$Status = as.factor(df$Status)
  df$Gender = as.factor(df$Gender)
  df$Licence = as.factor(df$Licence)
  df$Car_Avail = as.factor(df$Car_Avail)
  df$Work = as.factor(df$Work)
  df$Family = as.factor(df$Family)
  df$Level = as.factor(df$Level)
  
  ## Remove outliers
  # df_mode <- df[!(df$Time.Active > 600),]
  # df_mode <- df_mode[!(df_mode$Time.Transit > 200),]
  # df_mode <- df_mode[!(df_mode$Time.Auto > 150),]
  df_mode <- df[!(df$Time.Auto > 1e3),]
  
  df_mode_original = df_mode
  
  # Filter unavailable choices
  df_mode <- df_mode[!((df_mode$Mode == "Active") & df_mode$Time.Active >= 46.6),]
  #df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Car_Avail == 0),]
  #df_mode <- df_mode[!((df_mode$Mode == "Auto") & (df_mode$Cars == 0)),]
  df_mode <- df_mode[!((df_mode$Mode == "Auto") & df_mode$Cars <= 1),]
  
  
  # Transform dataframe from wide to long
  mldf = mlogit.data(df_mode, varying = 13:15, choice = "Mode", shape = "wide")
  
  # Availability Column
  mldf$available = (mldf$alt == "Auto" & mldf$Cars > 1) | (mldf$alt == "Transit") | (mldf$alt == "Active" & mldf$Time < 46.6)
  
  # Run Model
  model = mlogit(call, data = mldf, reflevel = "Transit", subset = mldf$available == 1)
  
  x = summary(model)
  #Results
  data = c()
  data[1] = sum(mldf$Mode & mldf$available) * mean(fitted(model)) / nrow(df_mode_original)
  data[2] = mean(fitted(model))
  data[3] = x[[20]][[1]]
  data[4] = nrow(df_mode)
  
  return(data)
}


# Formulas
formulas = list(c(mFormula(Mode ~ 0|1|Time)),
                c(mFormula(Mode ~ 0|Family|Time)),
                c(mFormula(Mode ~ 0|Children|Time)),
                c(mFormula(Mode ~ 0|Adults|Time)),
                c(mFormula(Mode ~ 0|Income|Time)),
                c(mFormula(Mode ~ 0|Family + Children|Time)),
                c(mFormula(Mode ~ 0|Family + Adults|Time)),
                c(mFormula(Mode ~ 0|Adults + Children|Time)),
                c(mFormula(Mode ~ 0|Family + Income|Time)),
                c(mFormula(Mode ~ 0|Family + Children + Adults|Time)),
                c(mFormula(Mode ~ 0|Family + Children + Income|Time)),
                c(mFormula(Mode ~ 0|Family + Adults + Income|Time)),
                c(mFormula(Mode ~ 0|Family + Children + Adults + Income|Time)),
                c(mFormula(Mode ~ 0|Licence|Time)),
                c(mFormula(Mode ~ 0|Income + Licence|Time)),
                c(mFormula(Mode ~ 0|Family + Licence|Time)),
                c(mFormula(Mode ~ 0|Family + Income + Licence|Time)),
                c(mFormula(Mode ~ 0|Family + Children + Adults + Income + Licence|Time)),
                c(mFormula(Mode ~ 0|Gender|Time)),
                c(mFormula(Mode ~ 0|Level|Time)),
                c(mFormula(Mode ~ 0|Status|Time)),
                c(mFormula(Mode ~ 0|Work|Time)),
                c(mFormula(Mode ~ 0|Gender + Level|Time)),
                c(mFormula(Mode ~ 0|Gender + Status|Time)),
                c(mFormula(Mode ~ 0|Gender + Work|Time)),
                c(mFormula(Mode ~ 0|Level + Work|Time)),
                c(mFormula(Mode ~ 0|Status + Work|Time)),
                c(mFormula(Mode ~ 0|Gender + Level + Work|Time)),
                c(mFormula(Mode ~ 0|Gender + Status + Work|Time)),
                c(mFormula(Mode ~ 0|Level + Family|Time)),
                c(mFormula(Mode ~ 0|Level + Income|Time)),
                c(mFormula(Mode ~ 0|Level + Family + Income|Time)),
                c(mFormula(Mode ~ 0|Work + Income|Time)),
                c(mFormula(Mode ~ 0|Work + Level + Income|Time)))

#run_model(formulas[[1]])

# ## Write resutls to .csv file:

results = array(numeric(), c(34,4))
for (i in 1:34) {
 data = run_model(formulas[[i]][[1]])
 results[i,1] = data[1]
 results[i,2] = data[2]
 results[i,3] = data[3]
 results[i,4] = data[4]
}

vars = c("None", "Family", "Children", "Adults", "Income", "Family + Children", "Family + Adults", "Adults + Children", "Family + Income",
         "Family + Children + Adults", "Family + Children + Income", "Family + Adults + Income", "Family + Children + Adults + Income",
         "Licence", "Income + Licence", "Family + Licence", "Family + Income + Licence", "Family + Children + Adults + Income + Licence",
         "Gender", "Level", "Status", "Work", "Gender + Level", "Gender + Status", "Gender + Work", "Level + Work", "Status + Work",
         "Gender + Level + Work", "Gender + Status + Work", "Level + Family", "Level + Income", "Level + Family + Income", "Work + Income",
         "Work + Level + Income")

d = as.data.frame(results)
setnames(d, old = c("V1", "V2", "V3", "V4"), new = c('Total Prob.','In-Sample Prob.', 'Biased R^2', 'Filtered Sample Size'))
write.csv(d, "Avail_Outputs_withChSpVars.csv", row.names = vars)

