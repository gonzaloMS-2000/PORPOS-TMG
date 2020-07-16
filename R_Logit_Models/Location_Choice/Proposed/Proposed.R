# ---- Packages ----
library(mlogit)
library(caret)
# library(CrossValidate)
# library(ClassComparison)
# library(Modeler)

# ---- Load and Format Data ----
setwd("C:/Users/gonza/Desktop/University/Summer 2020/TMG/GitHub_PORPOS/PORPOS-TMG/R_Logit_Models/Location_Choice/Campus Variables")
df <- read.csv("../../../Data/SMTO_2015/SMTO_2015_Complete_Input.csv")
df$School_Codes = as.factor(df$School_Codes)
mldf = mlogit.data(df, choice="School_Codes", shape="wide", varying = 18:87)
mldf$Domestic = mldf$Domestic * 100
mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))

# # ---- Previous Model ----
# model = mlogit(School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Domestic | Level,
#                data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
# print(summary(model))
# print(mean(fitted(model)))
# 
# # ---- Updated Model ----
# model = mlogit(School_Codes ~ Dist + Family:Dist + Level:Dist + Status:Dist + Family:Domestic + Enrol | 0,
#                data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
# print(summary(model))
# print(mean(fitted(model)))

#---- Simpler Model ----
model = mlogit(School_Codes ~ Dist + Family:Dist + Enrol | 0,
               data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
print(summary(model))
cat("APO:", mean(fitted(model)))


# ---- Model with k-Fold ----

x = df[18:24]
y = df$School_Codes
# 
# set.seed(42)
# model_k <- train(x = x, y = y,
#                 y~x,
#                 mldf,
#                 method = "lm",
#                 trControl = trainControl(
#                  method = "cv", number = 10,
#                  verboseIter = TRUE
#                ))

# cv = CrossValidate(model, mldf, status =  y,
#                     frac = 0.9, nLoop = 10, prune=keepAll, verbose=TRUE)a

# flds <- createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)

#Randomly shuffle the data
df <-df[sample(nrow(df)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(df)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df[testIndexes, ]
  trainData <- df[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  mldf = mlogit.data(trainData, choice="School_Codes", shape="wide", varying = 18:87)
  mldf$Enrol = ifelse(mldf$Level == "UG", mldf$UG, ifelse(mldf$Level == "Grad", mldf$Grad, mldf$Total))
  
  test_mldf = mlogit.data(testData, choice="School_Codes", shape="wide", varying = 18:87)
  test_mldf$Enrol = ifelse(test_mldf$Level == "UG", test_mldf$UG, ifelse(test_mldf$Level == "Grad", test_mldf$Grad, test_mldf$Total))
  
  model = mlogit(School_Codes ~ Dist + Family:Dist + Enrol | 0,
                 data=mldf, weights=mldf$Exp_Segment, reflevel="SG", subset=mldf$Segment!=0)
  actuals = testData$School_Codes
  #cat("APO:", mean(fitted(model)))
  
  probs = predict(model, newdata = test_mldf)
  print(000)
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(levels(actuals), levels(preds)))
  y =  suppressWarnings(confusionMatrix(preds, actuals))
  prec = sum(y[[4]][,5], na.rm=TRUE) / 7
  f1 = sum(y[[4]][,7], na.rm=TRUE) / 7
  metrics = c(y[[3]][1], prec, mean(y[[4]][,6]), f1, mcc(preds=preds, actuals=actuals), mean(fitted(model)) * length(fitted(model)) / length(actuals))
  names(metrics) = c("Acc", "Prec", "Rec", "F1", "MCC", "APO")
  print(metrics)
  
  
}
