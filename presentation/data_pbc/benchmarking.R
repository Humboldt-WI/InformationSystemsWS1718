# Title     : benchmarking
# Objective : benchmarking
# Created by: areiche
# Created on: 24.01.2018

## ROC and AUC, receiver operating characteristic and area under the curve
##Change status to be binary (1 alive, but liver transplantation, 0 is alive, 2 is dead)
pbc$status[which(pbc$status == 1)] <- 0
pbc$status[which(pbc$status == 2)] <- 1

##Adjust protime to account for NAs
pbc$protime[which(is.na(pbc$protime))] <- 0.1

##Use the predictions of the cox model and plot the ROC
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

##If we want to only accept a fpr of x percentage points, like 0.1, we need to first define a function pROC and then plot the graph.
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