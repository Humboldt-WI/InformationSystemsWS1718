# Title     : kaplan-meier
# Objective : kaplan-meier
# Created by: areiche
# Created on: 24.01.2018

## Load the package data
data(pbc, package="survival")

## Create a survival element by Surv function
days_of_survival <- with (pbc, Surv(pbc$time, pbc$status==2))
print(days_of_survival)

##K A P L A N - M E I E R
## Produce and plot Kaplan-Meier estimator by survfit function
model_fit_simple <- survfit(Surv(pbc$time, pbc$status == 2) ~ 1)
autoplot(model_fit_simple) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

##Show Kaplan-Meier estimates
summary(model_fit_simple)

##Plot differences on the different treatments
model_fit_trt <- survfit(Surv(pbc$time, pbc$status == 2) ~ pbc$trt)
autoplot(model_fit_trt) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

##Show Kaplan-Meier estimates
summary(model_fit_trt)

##Performing the Mantel-Haenzel test on conditional indepencence by function survdiff.
##Efficient in comparing groups that differ by categorical variables, but not continuous ones.
##The test statistic value is less than the critical value (using chi-square table) for
##degree of freedom equal to one. Hence, we can say that there is no significant difference
##between the two groups regarding the survival. We can follow that the actual treatment
##of these patients gave little impact on the survival chances.
survdiff(Surv(pbc$time, pbc$status==2)~pbc$trt)

##Plot differences on males and females and test log-rank test
model_fit_sex <- survfit(Surv(pbc$time, pbc$status == 2) ~ pbc$sex)
autoplot(model_fit_sex) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

survdiff(Surv(pbc$time, pbc$status==2)~pbc$sex)

##Plot differences on patience with hepatitis and without hepatitis and test log-rank test
model_fit_hepato <- survfit(Surv(pbc$time, pbc$status == 2) ~ pbc$hepato)
autoplot(model_fit_hepato) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

survdiff(Surv(pbc$time, pbc$status==2)~pbc$hepato)

##Using Survminer package, simple curve:
ggsurvplot(model_fit_trt, data = pbc)

##Curve containing risk table:
ggsurv <- ggsurvplot(model_fit_trt, data = pbc, title = "Survival Time of \n Biliary Cirrhosis Patients \n",
legend.title = "Treatment",
legend.labs = c("One", "Two"),
xlab = "\n Survival Time (days) ",
ylab = "Survival Probabilities \n",
risk.table = TRUE,
risk.table.title = "Risk Table",
conf.int = TRUE)
ggsurv