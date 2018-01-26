# Title     : kaplan-meier
# Objective : kaplan-meier
# Created by: areiche
# Created on: 24.01.2018

##K A P L A N - M E I E R
## Produce and plot Kaplan-Meier estimator by survfit function
model_fit <- survfit(Surv(pbc$time, pbc$status == 2) ~ 1)
autoplot(model_fit) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

##Plot differences on the different treatments
model_fit <- survfit(Surv(pbc$time, pbc$status == 2) ~ pbc$trt)
autoplot(model_fit) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

##Plot differences on males and females
model_fit <- survfit(Surv(pbc$time, pbc$status == 2) ~ pbc$sex)
autoplot(model_fit) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

##Plot differences on patience with hepatitis and without hepatitis
model_fit <- survfit(Surv(pbc$time, pbc$status == 2) ~ pbc$hepato)
autoplot(model_fit) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))


##Performing the Mantel-Haenzel test on conditional indepencence by function survdiff.
##Efficient in comparing groups that differ by categorical variables, but not continuous ones.
##We can follow that the actual treatment of these patients gave little impact on the survival chances as we reject the test.
survdiff(Surv(pbc$time, pbc$status==2)~pbc$trt)