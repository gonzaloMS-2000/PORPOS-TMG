# ---- Packages ----
library(data.table)
library(mlogit)
library(caret)
library(e1071)
library(mltools)



# ---- Load and Format Data ----
#setwd("C:/Users/ethan/Documents/Ethan/TMG/Research/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df$School = as.factor(df$School)
mldf = mlogit.data(df, choice="School", shape="wide", varying = 18:87)
mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))

# ---- Variables ----
num_campuses = 7L
num_metrics = 5L # Accuracy, Macro PRF, MCC

# ---- Run Model ----
for (j in 0:0)
{
  actuals = subset(df, Segment!=0)$School
  model = mlogit(School ~ Dist + Enrol + Dist:Family| 0, data=mldf,
                 weights=mldf$Exp_Segment, reflevel="SG",
                 subset= (mldf$Segment!=0))
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
  print(summary(model))
  print(metrics)
}

#### ----- Calculating Absolute Residuals per Zone ----- [Residual = abs((Predicted - Observed)/Total)]

hz = sort(unique(df$HomeZone))
residuals = matrix(data=NA, nrow = length(hz), ncol = (length(campuses) + 1))
campuses = names(as.data.frame(probs))
probs_indices = as.numeric(row.names(probs)) # get list of actual row number-names (NOT POSITIONS) for our Segment != 0 subset (INDEX and ROWNAMES DONT MATCH!!!).
                                             # this array is necessary in order to get the position of a row based on its row name.  

for (zone_idx in 1:length(hz)){
  print(hz[zone_idx])
  temp_df = subset(df, Segment!=0)[subset(df, Segment!=0)$HomeZone == hz[zone_idx],] # --- Get 'temp_df': subset of observations from specific zone --- (Segment 0 is always removed)
  row_names = as.numeric(rownames(temp_df)) # Get row names (NOT INDICES) for our temp_df
  indices = which(probs_indices %in% row_names) # Get row INDICES for our temp_df with the help of probs_indices above
  print(indices)
  
  # --- We need a temp_probs dataframe which contains the probabilities for each row that belongs to the specific home zone
  if (length(indices) == 1){     # if there is only one person living in this zone, then we only have one observation and by default R eliminates the row name
    print('Index length = 1')       # so we place it back with these two steps
    temp_probs = t(as.data.frame(probs[indices,]))
    rownames(temp_probs) = row_names
  }
  else{   # if we have more than one observation, then R will keep the indeces as a normal dataframe, so temp_probs is just = probs[indices,]   
    print('Index Length NOT = 1')
    temp_probs = (probs[indices,])
  }
  print(temp_probs)
  #total = 0
  cm = matrix(data=NA, nrow = length(campuses), ncol = length(campuses)) # allocate 7x7 matrix for confusion matrix
  
  # --- Now for each observed school subset we need to add up the probabilities that that student (from a specific school) goes to any of the other schools --> this is how we get the softmax
  for (i in 1:length(campuses)){ 
    print(campuses[i])
    z = temp_probs[which(temp_df$School == campuses[i]),] # z is the subset of rows from our temp_probs where the student goes to the speficified school
                                                          # Note that some of the subsets may be empty if there are no students going to one of the schools.
    print(length(z))
    if (length(z) == 7){ # if our 'z' df only has one row...
      print(z)
      cm[i,] = z   # ... then our confusion matrix row for that school is simply that 'z' row
    }
    else{   # if z has more than one row...
      print(z)
      cm[i,] = colSums(z) #then we need to add the rows by column (ie. per predicted school) and the resulting row will be placed in the confusion matrix
    }
  }
  print('Confusion Matrix below')
  print(cm)
  
  # Now populate the residuals matrix: for this home zone's confusion matrix, we get the (Predicted - Observed)/Total for each school.
  pred_obs = matrix(data=NA, nrow = 2, ncol = length(campuses))
  pred_obs[1,] = colSums(cm) # Predicted for each school
  pred_obs[2,] = rowSums(cm) # Observed for each school
  residuals[zone_idx,1] = hz[zone_idx]
  print('Almost There')
  for (i in 1:length(campuses)){
    residuals[zone_idx,i+1] = abs((pred_obs[1,i] - pred_obs[2,i])/rowSums(pred_obs)[1])
  }
  print('ROUND FINISHED')
}

#write.table(residuals, '../Proposed/Residuals_All_Zones.csv', sep = ",", col.names = c('Zone', campuses))


