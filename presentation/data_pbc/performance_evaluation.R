# Title     : performance_evalutation
# Objective : performance_evaluation
# Created by: areiche
# Created on: 30.01.2018

##ROC and AUC, Receiver operating characteristic and Area under the curve
pred <- prediction(predict(cox_model), pbc$status)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

##Sensitivity: Probability that a person with the disease will have a positive test result
##Specificity: Probability that a person without the disease will have a negative test result.
##At every cutoff, the TPR and FPR are calculated and plotted. The smoother the graph, the more cutoffs the predictions have.
##We also plotted a 45-degree line, which represents, on average, the performance of a Uniform(0, 1) random variable.
##The further away (towards (0,1) from the diagonal line, the better.
##Overall, we see that we see gains in sensitivity (true positive rate, (> 80%)), trading off a false positive rate (1- specificity),
##up until about 25% FPR. After an FPR of 25%, we don't see significant gains in TPR for a tradeoff of increased FPR.

##If we want to only accept a for of x percentage points, like 0.1, we need to first define a function pROC and then plot the graph.
##As result, if we can only accept a FPR of 10%, the model is only giving 50% sensitivity (TPR) at 10% FPR (1-specificity).
pROC = function(pred, fpr.stop){
    perf <- performance(pred,"tpr","fpr")
    for (iperf in seq_along(perf@x.values)){
        ind = which(perf@x.values[[iperf]] <= fpr.stop)
        perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
        perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
    }
    return(perf)
}

##Plotting the new line
proc.perf = pROC(pred, fpr.stop=0.1)
plot(proc.perf)
abline(a=0, b= 1)

##Accessing the odds of a single person/data point in the data set to predict its individual survival:
curves <- survfit(cox_model, NULL)
curves[10]
curves[100]
curves[200]
plot(curves[10])
plot(curves[100])
plot(curves[200])

##To add entirely new data and see its performance, we have to enrich X
curves <- survfit(cox_model, newdata = X)
plot(survfit(cox_model2, newdata=data.frame(age=60,edema=2.0,bili=2.6,albumin=1.3,protime=3)),
xlab = "Days", ylab="Survival")

##Hence, there are 3 steps for predicting survival, using a Cox model:
##First, run the coxph ()
##Second, run survfit on the outcome of step one to generate survival curves - mind that we receive not singles but a set
##Third, print the plots

##Receive the AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

##Partial AUC, this value can range from 0 to infinity
pauc.perf = performance(pred, measure = "auc", fpr.stop=0.1)
pauc.perf@y.values

##Divide pAUC by fpr.stop to receive a standardized result form 0 to 1 like AUC
pauc.perf@y.values = lapply(pauc.perf@y.values, function(x) x / 0.1)
pauc.perf@y.values