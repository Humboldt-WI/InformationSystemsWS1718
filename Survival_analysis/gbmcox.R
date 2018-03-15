memory.limit(30000)
if(!require("gbm")) install.packages("gbm"); library("gbm")
if(!require("KMsurv")) install.packages("KMsurv"); library("KMsurv")
if(!require("survAUC")) install.packages("survAUC"); library("survAUC")
if(!require("survival")) install.packages("survival"); library("survival")
  
#for hdd dataset

#Loading the data
#c <- readRDS("./Survival_analysis/data_2016_rds/hdd_onemeas.rds")
#c <- readRDS("./Survival_analysis/data_2016_rds/hdd_Dataset.rds")
#cox.R must have run to create test and trainset!

#gbm does not support intervall partitioned data. therefore  we look at the full timespan

train.coxgbm <- coxph(Surv( date, failure) ~ smart_9_normalized + smart_5_normalized +smart_187_normalized 
                      +smart_188_normalized+smart_197_normalized + smart_198_normalized,
                      data = trainset)





#we don't have startdate here
gbmhdd = gbm(train.coxgbm,
             data = trainset,
             distribution = "coxph",
             n.trees = 2500,
             shrinkage = 0.02,
             n.minobsinnode = 4)
summary(gbmhdd)

gbmtrainhdd = predict(object = gbmhdd,
                      newdata = trainset,
                      n.trees = 1500,
                      type = "response")


gbmtesthdd = predict(object = gbmhdd,
                     newdata = testset,
                     n.trees = 1500,
                     type = "response")


Survresptrainhddgbm <- Surv(trainset$date,trainset$failure)
Survresptesthddgbm <- Surv(testset$date,testset$failure)
CI_gbmhdd <- BeggC(Survresptrainhddgbm, Survresptesthddgbm, gbmtrainhdd, gbmtesthdd)
if(CI_gbmhdd<=0.5){
  CI_gbmhdd =1-CI_gbmhdd
}






#for pbc dataset


#cox.R must have run to create train test, and coxph object!

gbmpbc = gbm(pbc.cox,
             data = trainpbc,
             distribution = "coxph",
             n.trees = 2500,
             shrinkage = 0.02,
             n.minobsinnode = 4)


summary(gbmpbc)



gbmtrainpbc = predict(object = gbmpbc,
                      newdata = trainpbc,
                      n.trees = 1500,
                      type = "response")


gbmtestpbc = predict(object = gbmpbc,
                     newdata = testpbc,
                     n.trees = 1500,
                     type = "response")


Survresptrainpbc <- Surv(trainpbc$time,trainpbc$status==2)
Survresptestpbc <- Surv(testpbc$time,testpbc$status == 2)
CI_gbmpbc <- BeggC(Survresptrainpbc, Survresptestpbc, gbmtrainpbc, gbmtestpbc)
if(CI_gbmpbc<=0.5){
  CI_gbmpbc =1-CI_gbmpbc
}
CI_gbmpbc