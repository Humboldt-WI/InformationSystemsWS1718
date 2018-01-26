# Title     : cox_model
# Objective : cox_model
# Created by: areiche
# Created on: 24.01.2018

##Producing a Cox Model, also known as proportional hazard model by function coxph.
##Allows to access effects of continuous and categorical elements.
##Uses partial likelihood to access the inference, the knowledge of actual baseline of hazard is not required
##However, model implies strong assumptions (tbd).
cox_model <- coxph(Surv(pbc$time, pbc$status == 2) ~ pbc$age + pbc$edema + log(pbc$bili) + log(pbc$albumin) + log(pbc$protime))
summary(cox_model)

##The 2nd and 3rd model is just for our interest. By strata(element), we assign the survival ultimatively to a variable.
##Each strata has a different baseline hazard function but the remaining covariates are assumed to be constant.
cox_model_2 <- coxph(Surv(pbc$time, pbc$status == 2) ~ pbc$age + strata(pbc$edema) + log(pbc$bili) + log(pbc$albumin) + log(pbc$protime))
cox_model_3 <- coxph(Surv(pbc$time, pbc$status == 2) ~ pbc$age + pbc$edema +strata(pbc$stage) + log(pbc$bili) + log(pbc$albumin) + log(pbc$protime))

##For plotting mean covariates of Cox, hence the chances of survival for a "mean" patient with liver cancer
plot(survfit(cox_model), lwd = 2, main = 'Fitted survival function at mean covariates', xlab = 'Days', ylab = 'Survival')

##Diagnostics of Cox
##Estimating the performance of the Cox model by Schoenfeld residuals.
##We investigate the ability of the model to fit to the given subjects by testing the proportional hazard assumptions on our cox_model.
cox.zph(cox_model, transform = "identity")
a <- cox.zph(cox_model)
par(mfrow = c(3, 2))
plot(a[1], main = "Scaled Schoenfeld Residuals Plot")
plot(a[2], main = "Scaled Schoenfeld Residuals Plot")
plot(a[3], main = "Scaled Schoenfeld Residuals Plot")
plot(a[4], main = "Scaled Schoenfeld Residuals Plot")
plot(a[5], main = "Scaled Schoenfeld Residuals Plot")