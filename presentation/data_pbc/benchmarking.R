# Title     : benchmarking
# Objective : benchmarking
# Created by: areiche
# Created on: 24.01.2018

##                                  Predicting the future outlook
##The cumulative incidence curve is an alternative to the Kaplan-Meier for competing risks data.
##For instance, in patients with MGUS, conversion to an overt plasma cell malignancy occurs at a nearly
##constant rate among those still alive. A Kaplan-Meier estimate, treating death due to other causes as censored,
##gives a 20 year cumulate rate of 33% for the 241 early patients of Kyle. This estimates the incidence of
##conversion if all other causes of death were removed, which is an unrealistic assumption given the mean
##starting age of 63 and a median follow up of over 21 years.

##The CI estimate, on the other hand, estimates the total number of conversions that will actually occur.
##Because the population is older, this is much smaller than the KM, 22% at 20 years for Kyle's data.
##If there were no censoring, then CI(t) could very simply be computed as total number of patients with
##progression by time t divided by the sample size n.

fitKM <- survfit(Surv(stop, event=='pcm') ~1, data=mgus1,
subset=(start==0))

fitCI <- survfit(Surv(stop, status*as.numeric(event), type="mstate") ~1,
data=mgus1, subset=(start==0))

# CI curves are always plotted from 0 upwards, rather than 1 down
plot(fitCI, xscale=365.25, xmax=7300, mark.time=FALSE,
col=2:3, xlab="Years post diagnosis of MGUS")
lines(fitKM, fun='event', xmax=7300, mark.time=FALSE,
conf.int=FALSE)
text(10, .4, "Competing risk: death", col=3)
text(16, .15,"Competing risk: progression", col=2)
text(15, .30,"KM:prog")


##Optimal Cutoff Point
##Is the point that gives maximum correct classification.
##Each observation in the data, generates a binary response classification matrix in form of predicted probability of the positive
##or negative result. Predicted probability is a continuous value between 0 and 1. But it is desired to have a binary prediction
##whether test result is positive or negative. This leads to choose a cut-off point on probability scale where if the  predicted
##probability exceeds the chosen cut-off point, then the result is test positive. Otherwise, the predicted response is test negative.

##There are different methods to obtain this cut-off point.
##1. Minimize  Cost  criterion:
##This  method  considers  the  financial  cost,  health  impact,  discomfort  to  patient  and  further investigative  cost  (downstream  cost)  for  correct  and  false diagnosis.
##These  also  considers  factors  like  disease prevalence and prior probability. Other methods to obtain cut-off point are based upon sensitivity and specificity.
##They give equal weight-age to sensitivity and specificity.

##2. Youden Index (Used in ROCR):
##Maximum (Sensitivity + Specificity – 1)
##Youden index (J) is maximum potential of effectiveness of diagnostic test. This maximizes the distance between chance diagonal to the point [x, y] on the curve, a point farthest from random.
##It ranges between 0 to 1. Where J=1 indicates that there is no false negative rate (FNR) or false positive rate(FPR), i.e. effectiveness of diagnostic test is perfect, and
##J=0 indicates that diagnostic test gives the same proportion of positive results for the group with and without disease, i.e. test is useless.

##3.Distance to (0, 1):
##SQRT ((1 - Sensitivity)² + (1 - Specificity)²)
##Distance to (0,1), D is point closest to ideal point. This minimizes the distance from the "perfect" point at the upper-left corner of the ROC plot
##where 1 - Specificity=0 and Sensitivity=1. The point with shortest distance is D, test result value corresponding to this point is threshold cut-off point.

##4.Sensitivity, Specificity equality:
##ABS(Sensitivity - Specificity)
##This method of identifying cut-off point minimizes the difference between sensitivity and specificity values at all possible cut-off  points.
##The point between lowest  absolute  difference  between  sensitivity  and  specificity is  the equality point. And the test result value corresponding to this is threshold cut-off point.

##The cut point is “optimal” in the sense it weighs both sensitivity and specificity equally.
##If we want to get the optimal cut-out, we can use the following code.
##The code takes in BOTH the performance object and prediction object and gives the optimal cutoff value of your predictions::
opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
        d = (x - 0)^2 + (y-1)^2
        ind = which(d == min(d))
        c(sensitivity = y[[ind]], specificity = 1-x[[ind]],
        cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

##It can happen that we have different costs for false negative and false positive
##The output from opt.cut and a performance object with measure cost are NOT equivalent if false positives and false negatives
##are not weighted equally. The cost.fn and cost.fp arguments can be passed to performance, corresponding to the cost of a false negative
##and false positive, respectively (This is minimize cost criterion).
cost.perf = performance(pred, "cost", cost.fp = 2, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

##Plotting the cost line, once weighted and once by the ROCR standard cost.
plot(performance(pred,"cost"))
plot(cost.perf)


##Receive the AUC - ARVID TO DO BENCHMARKING OF ALL MODELS




##Tools for visualizing, smoothing and comparing receiver operating characteristic (ROC curves).
##(Partial) area under the curve (AUC) can be compared with statistical tests based on U-statistics or bootstrap.
##Confidence intervals can be computed for (p)AUC or ROC curves.