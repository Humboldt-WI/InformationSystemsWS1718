memory.limit(30000)
if("package:gbm" %in% search()) detach("package:gbm", unload=TRUE) 
install.packages("C:/Users/B_JD/Desktop/GBMCI-master", repos = NULL, type = "source")
library("gbm")
if(!require("KMsurv")) install.packages("KMsurv"); library("KMsurv")
if(!require("survAUC")) install.packages("survAUC"); library("survAUC")
#if(!require("survival")) install.packages("survival"); library("survival")





#for pbc dataset


#cox.R must have run to create train test, and coxph object!


gbmcipbc = gbm(formula =Surv(trainpbc$time,trainpbc$status ==2 )~ coxpredicted_trainpbc,
             distribution = "sci",
             n.trees = 2500,
             shrinkage = 1,
             n.minobsinnode = 4)


  summary(gbmcipbc)


gbmcitrainpbc = predict(object = gbmcipbc,
                      newdata = trainpbc,
                      n.trees = 1500,
                      type = "response")

#gives error: Warning message:
#'newdata' hat 104 Zeilen , aber die gefundenen Variablen haben 312 Zeilen 
#'#seems to be a bug as it doesn't happen in any other case
#'r believes the predictor in the gbm model, here ~ coxpredicted_trainpbbc comes from the dataset
#'that the response variables are part of. in fact its the cumulated relative hazards from the pbc.cox model
gbmcitestpbc = predict(object = gbmcipbc,
                     newdata = testpbc,
                     n.trees = 1500,
                     type = "response")


Survresptrainpbcci <- Surv(trainpbc$time,trainpbc$status==2)
Survresptestpbcci <- Surv(testpbc$time,testpbc$status == 2)
CI_gbmcipbc <- BeggC(Survresptrainpbcci, Survresptestpbcci, gbmcitrainpbc, gbmcitestpbc)
if(CI_gbmcipbc<=0.5){
  CI_gbmcipbc =1-CI_gbmcipbc
}
CI_gbmcipbc