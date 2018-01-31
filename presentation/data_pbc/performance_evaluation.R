# Title     : performance_evalutation
# Objective : performance_evaluation
# Created by: areiche
# Created on: 30.01.2018






##Receive the AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

##Partial AUC, this value can range from 0 to infinity
pauc.perf = performance(pred, measure = "auc", fpr.stop=0.1)
pauc.perf@y.values

##Divide pAUC by fpr.stop to receive a standardized result form 0 to 1 like AUC
pauc.perf@y.values = lapply(pauc.perf@y.values, function(x) x / 0.1)
pauc.perf@y.values