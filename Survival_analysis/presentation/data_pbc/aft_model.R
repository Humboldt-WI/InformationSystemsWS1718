# Title     : aft_model
# Objective : aft_model
# Created by: areiche
# Created on: 24.01.2018

##First adjust data quickly
##Change status to be binary (1 alive, but liver transplantation, 0 is alive, 2 is dead)
pbc$status[which(pbc$status == 1)] <- 0
pbc$status[which(pbc$status == 2)] <- 1

##Adjust protime to account for NAs
pbc$protime[which(is.na(pbc$protime))] <- 0.1

##Modelling ATF Model
atf_model_weibull <- survreg(Surv(pbc$time, pbc$status == 2) ~ pbc$age + pbc$edema + log(pbc$bili) + log(pbc$albumin) + log(pbc$protime), dist='weibull')
summary(atf_model_weibull)

atf_model_exponential <- survreg(Surv(pbc$time, pbc$status == 2) ~ pbc$age + pbc$edema + log(pbc$bili) + log(pbc$albumin) + log(pbc$protime), dist='exponential')
summary(atf_model_exponential)

atf_model_log <- survreg(Surv(pbc$time, pbc$status == 2) ~ pbc$age + pbc$edema + log(pbc$bili) + log(pbc$albumin) + log(pbc$protime), dist='loglogistic')
summary(atf_model_log)

##Receive AUC and ROC plot for all ATF models
par(mfrow = c(2, 2))
pred_atf <- prediction(predict(atf_model_weibull), pbc$status)
perf_atf <- performance(pred_atf,"tpr","fpr")
plot(perf_atf)
abline(a=0, b= 1)
auc.perf_atf_weibull = performance(pred_atf, measure = "auc")
auc.perf_atf_weibull@y.values

pred_exponential <- prediction(predict(atf_model_exponential), pbc$status)
perf_exponential <- performance(pred_exponential,"tpr","fpr")
plot(perf_exponential)
abline(a=0, b= 1)
auc.perf_atf_exponential = performance(pred_exponential, measure = "auc")
auc.perf_atf_exponential@y.values

pred_log <- prediction(predict(atf_model_log), pbc$status)
perf_log <- performance(pred_log,"tpr","fpr")
plot(perf_log)
abline(a=0, b= 1)
auc.perf_atf_log = performance(pred_log, measure = "auc")
auc.perf_atf_log@y.values