library(caret)
library(mltools)

hardmax_preds = function(probs, real_levels) # Hardmax: take highest probability
{
  preds = vector()
  for (i in 1:nrow(probs)) preds = c(preds, colnames(probs)[which.max(probs[i,])[[1]]])
  preds = as.factor(preds)
  levels(preds) = c(levels(preds), setdiff(real_levels, levels(preds)))
  return(preds)
}

softmax_cm = function(probs, actuals, real_levels) # Softmax: Add up probabilities
{
  l = length(real_levels)
  cm = matrix(data=NA, nrow=l, ncol=l)
  for (i in 1:l) cm[i,] = colSums(probs[which(actuals == real_levels[i]),])
  return(cm)
}

get_cm = function(preds, actuals)
  suppressWarnings(confusionMatrix(preds, actuals))

print_cm = function(cm)
  cm[[2]]

get_prec = function(cm)
  sum(cm[[4]][,5], na.rm=TRUE) / nrow(cm[[4]])

get_rec = function(cm)
  mean(cm[[4]][,6])

get_f1 = function(cm)
  sum(cm[[4]][,7], na.rm=TRUE) / nrow(cm[[4]])

get_accuracy = function(cm)
  cm[[3]][1]

get_softmax_accuracy = get_apo = function(model) # Previously called APO
  mean(fitted(model))

get_probs = function(model)
  fitted(model, outcome = FALSE)

get_log_lik = function(model)
  model[[2]][1]

get_mcc = function(preds, actuals)
  mcc(preds=preds, actuals=actuals)