# Title     : benchmarking
# Objective : benchmarking
# Created by: areiche
# Created on: 24.01.2018

##                                  How to ANALYSE and ACCESS a single subject
## Accessing the odds of a single person/data point in the data set to predict its individual survival:
data(pbc)
cox_model <- coxph(Surv(time, status == 2) ~ age + edema + log(bili) + log(albumin) + log(protime), data=pbc)
summary(cox_model)
curves <- survfit(cox_model, pbc)
curves[10]
curves[100]
curves[200]
par(mfrow = c(2, 2))
plot(curves[10], xlab = "Days", ylab="Survival Probability")
plot(curves[100], xlab = "Days", ylab="Survival Probability")
plot(curves[200], xlab = "Days", ylab="Survival Probability")

## To add entirely new data and see its performance, we have to enrich X
## curves <- survfit(cox_model, newdata = X)
plot(survfit(cox_model2, newdata=data.frame(age=60,edema=2.0,bili=2.6,albumin=1.3,protime=3)),
xlab = "Days", ylab="Survival")


##                                  Predicting the future outlook
## The cumulative incidence curve is an alternative to the Kaplan-Meier for competing risks data.
## A Kaplan-Meier estimate, treating death due to other causes as censored, gives a 12 year cumulate rate of 10%
## for the 424 patients of PBC. The CuIn estimate, on the other hand, estimates the total number of conversions
## that will actually occur. Because the population is older, this is smaller than the KM,
## 7% at 12 years for PBC's data. If there were no censoring, then CuIn(t) could very simply be computed as
## total number of patients with progression by time t divided by the sample size n.

par(mfrow = c(1, 1))
fitKM <- survfit(Surv(time, status==1) ~1, data=pbc)

fitCI <- survfit(Surv(time, status, type="mstate") ~1,
data=pbc)

# CI curves are always plotted from 0 upwards, rather than 1 down
plot(fitCI, xscale=365.25, xmax=7300, mark.time=FALSE,
col=2:3, xlab="Years post diagnosis of PBC")
lines(fitKM, fun='event', xmax=7300, mark.time=FALSE,
conf.int=FALSE)
text(500, .4, "Competing risk: death", col=3)
text(500, .15,"Competing risk: progression", col=2)
text(500, .30,"Kaplan Meier: prog")


##                                              Benchmarking Methods
## ROC - Receiver operating characteristic
## First, change status to be binary (1 alive, but liver transplantation, 0 is alive, 2 is dead)
pbc$status[which(pbc$status == 1)] <- 0
pbc$status[which(pbc$status == 2)] <- 1

## Generate predictions of cox_model and performance variable and plot the ROC
pred <- prediction(predict(cox_model), pbc$status)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b= 1)

## Sensitivity: Probability that a person with the disease will have a positive test result
## Specificity: Probability that a person without the disease will have a negative test result.
## At every cutoff, the TPR and FPR are calculated and plotted. The smoother the graph, the more cutoffs the predictions have.
## We also plotted a 45-degree line, which represents, on average, the performance of a Uniform(0, 1) random variable.
## The further away (towards (0,1) from the diagonal line, the better.
## Overall, we see that we see gains in sensitivity (true positive rate, (> 80%)), trading off a false positive rate (1- specificity),
## up until about 25% FPR. After an FPR of 25%, we don't see significant gains in TPR for a tradeoff of increased FPR.

## AUC - Area under the curve
## Receive the AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

##                                                      Benchmark the models
## Training a data set
data(pbc,package = "survival")
pbc_clean <- pbc[!is.na(pbc$age)
& !is.na(pbc$edema)
& !is.na(pbc$bili)
& !is.na(pbc$albumin)
& !is.na(pbc$protime)
& !is.na(pbc$status)
,]

# Selecting 33%, 66% and 100%  from initial population
samplepbc <- sample.int(n = NROW(pbc_clean), size = floor(.33*NROW(pbc_clean)), replace = F)
trainpbc_1_3 <- pbc_clean[samplepbc,]
testpbc_2_3  <- pbc_clean[-samplepbc,]
samplepbc <- sample.int(n = NROW(pbc_clean), size = floor(.66*NROW(pbc_clean)), replace = F)
trainpbc_2_3 <- pbc_clean[samplepbc,]
testpbc_1_3  <- pbc_clean[-samplepbc,]

# Building the trained proportional hazard cox models
pbc.cox_1_3 <- coxph(Surv(time,status ==2 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc_1_3)
pbc.cox_2_3 <- coxph(Surv(time,status ==2 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc_2_3)
pbc.cox_3_3 <- coxph(Surv(time,status ==2 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = pbc_clean)

## Building a gradient boosting model from the above mentioned cox model
gbmpbc_1_3 = gbm(pbc.cox_1_3,
data = trainpbc_1_3,
distribution = "coxph",
n.trees = 2500,
shrinkage = 0.02,
n.minobsinnode = 4)

gbmpbc_2_3 = gbm(pbc.cox_2_3,
data = trainpbc_2_3,
distribution = "coxph",
n.trees = 2500,
shrinkage = 0.02,
n.minobsinnode = 4)

gbmpbc_3_3 = gbm(pbc.cox_3_3,
data = pbc_clean,
distribution = "coxph",
n.trees = 2500,
shrinkage = 0.02,
n.minobsinnode = 4)

## Modelling the ATF models
atf_model_weibull_1_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='weibull', data = trainpbc_1_3)
atf_model_weibull_2_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='weibull', data = trainpbc_2_3)
atf_model_weibull_3_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='weibull', data = pbc_clean)

atf_model_exponential_1_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='exponential', data = trainpbc_1_3)
atf_model_exponential_2_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='exponential', data = trainpbc_2_3)
atf_model_exponential_3_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='exponential', data = pbc_clean)

atf_model_log_1_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='loglogistic', data = trainpbc_1_3)
atf_model_log_2_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='loglogistic', data = trainpbc_2_3)
atf_model_log_3_3 <- survreg(Surv(time, status == 2) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='loglogistic', data = pbc_clean)


##                                          Receive AUCs for all models
## First, generate a binary variable again
trainpbc_1_3$status[which(trainpbc_1_3$status == 1)] <- 0
trainpbc_1_3$status[which(trainpbc_1_3$status == 2)] <- 1
trainpbc_2_3$status[which(trainpbc_2_3$status == 1)] <- 0
trainpbc_2_3$status[which(trainpbc_2_3$status == 2)] <- 1
pbc_clean$status[which(pbc_clean$status == 1)] <- 0
pbc_clean$status[which(pbc_clean$status == 2)] <- 1

## Now generate predictions on all models
pred_coxph_1_3 <- prediction(predict(pbc.cox_1_3), trainpbc_1_3$status)
pred_coxph_2_3 <- prediction(predict(pbc.cox_2_3), trainpbc_2_3$status)
pred_coxph_3_3 <- prediction(predict(pbc.cox_3_3), pbc_clean$status)

pred_gbmpbc_1_3 = prediction(predict(object = gbmpbc_1_3,
newdata = trainpbc_1_3,
n.trees = 1500,
type = "response"), trainpbc_1_3$status)
pred_gbmpbc_2_3 = prediction(predict(object = gbmpbc_2_3,
newdata = trainpbc_2_3,
n.trees = 1500,
type = "response"), trainpbc_2_3$status)
pred_gbmpbc_3_3 = prediction(predict(object = gbmpbc_3_3,
newdata = pbc_clean,
n.trees = 1500,
type = "response"), pbc_clean$status)

pred_atf_model_weibull_1_3 <- prediction(predict(atf_model_weibull_1_3), trainpbc_1_3$status)
pred_atf_model_weibull_2_3 <- prediction(predict(atf_model_weibull_2_3), trainpbc_2_3$status)
pred_atf_model_weibull_3_3 <- prediction(predict(atf_model_weibull_3_3), pbc_clean$status)

pred_atf_model_exponential_1_3 <- prediction(predict(atf_model_exponential_1_3), trainpbc_1_3$status)
pred_atf_model_exponential_2_3 <- prediction(predict(atf_model_exponential_2_3), trainpbc_2_3$status)
pred_atf_model_exponential_3_3 <- prediction(predict(atf_model_exponential_3_3), pbc_clean$status)

pred_atf_model_log_1_3 <- prediction(predict(atf_model_log_1_3), trainpbc_1_3$status)
pred_atf_model_log_2_3 <- prediction(predict(atf_model_log_2_3), trainpbc_2_3$status)
pred_atf_model_log_3_3 <- prediction(predict(atf_model_log_3_3), pbc_clean$status)


## Now receive the AUC values for each model
auc.perf_coxph_1_3 = performance(pred_coxph_1_3, measure = "auc")
auc.perf_coxph_2_3 = performance(pred_coxph_2_3, measure = "auc")
auc.perf_coxph_3_3 = performance(pred_coxph_3_3, measure = "auc")

auc.perf_gbmpbc_1_3 = performance(pred_gbmpbc_1_3, measure = "auc")
auc.perf_gbmpbc_2_3 = performance(pred_gbmpbc_2_3, measure = "auc")
auc.perf_gbmpbc_3_3 = performance(pred_gbmpbc_3_3, measure = "auc")

auc.perf_atf_model_weibull_1_3 = performance(pred_atf_model_weibull_1_3, measure = "auc")
auc.perf_atf_model_weibull_2_3 = performance(pred_atf_model_weibull_2_3, measure = "auc")
auc.perf_atf_model_weibull_3_3 = performance(pred_atf_model_weibull_3_3, measure = "auc")

auc.perf_atf_model_exponential_1_3 = performance(pred_atf_model_exponential_1_3, measure = "auc")
auc.perf_atf_model_exponential_2_3 = performance(pred_atf_model_exponential_2_3, measure = "auc")
auc.perf_atf_model_exponential_3_3 = performance(pred_atf_model_exponential_3_3, measure = "auc")

auc.perf_atf_model_log_1_3 = performance(pred_atf_model_log_1_3, measure = "auc")
auc.perf_atf_model_log_2_3 = performance(pred_atf_model_log_2_3, measure = "auc")
auc.perf_atf_model_log_3_3 = performance(pred_atf_model_log_3_3, measure = "auc")



##                                      Final Benchmarking and Future Outlook
## It can happen that we have different costs for false negative and false positive
## The output from opt.cut and a performance object with measure cost are NOT equivalent if false positives and false negatives
## are not weighted equally. The cost.fn and cost.fp arguments can be passed to performance, corresponding to the cost of a false negative
## and false positive, respectively (This is minimize cost criterion).
cost.perf = performance(pred, "cost", cost.fp = 2, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

## Plotting the cost line, once weighted and once by the ROCR standard cost.
plot(performance(pred,"cost"))
plot(cost.perf)

## If we want to only accept a for of x percentage points, like 0.1 as false correct prediction, we need to first
## define a function pROC and then plot the graph.
## As result, if we can only accept a FPR of 10%, the model is only giving 50% sensitivity (TPR) at 10% FPR (1-specificity).
pROC = function(pred, fpr.stop){
    perf <- performance(pred,"tpr","fpr")
    for (iperf in seq_along(perf@x.values)){
        ind = which(perf@x.values[[iperf]] <= fpr.stop)
        perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
        perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
    }
    return(perf)
}

## Plotting the new line
proc.perf = pROC(pred, fpr.stop=0.1)
plot(proc.perf)
abline(a=0, b= 1)

## Address now for evaluation the partial AUC, this value can range from 0 to infinity
pauc.perf = performance(pred, measure = "auc", fpr.stop=0.1)
pauc.perf@y.values

## Divide pAUC by fpr.stop to receive a standardized result form 0 to 1 like AUC
pauc.perf@y.values = lapply(pauc.perf@y.values, function(x) x / 0.1)
pauc.perf@y.values