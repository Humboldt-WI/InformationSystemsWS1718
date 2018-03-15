
memory.limit(30000)
if(!require("survival")) install.packages("survival"); library("survival")
if(!require("survminer")) install.packages("survminer"); library("survminer")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("ggfortify")) install.packages("ggfortify"); library("ggfortify")
if(!require("survAUC")) install.packages("survAUC"); library("survAUC")
if(!require("pec")) install.packages("pec"); library("pec")
if(!require("timereg")) install.packages("timereg"); library("timereg")
if(!require("KMsurv")) install.packages("KMsurv"); library("KMsurv")
  
#Loading the data
c <- readRDS("./data_2016_rds/hdd_Dataset.rds")

#c <- readRDS("./data_2016_rds/hdd_onemeas.rds")# remove startdate from Survobjects when using this!

#Kaplan Meier
# to indicate begin of each measuring period we specifiy a startdate
KM_data <- c
KM_data$startdate <- 0
KM_data[KM_data$date ==1,]$startdate <- 0
KM_data[KM_data$date ==90,]$startdate <- 1
KM_data[KM_data$date ==180,]$startdate <-90
KM_data[KM_data$date ==270,]$startdate <- 180
KM_data[KM_data$date ==366,]$startdate <- 270

#creating the survival object
with(KM_data, Surv(startdate,date, failure))
KM_fitted <- survfit(Surv(startdate,date, failure) ~ 1, data=KM_data)
summary(KM_fitted)
autoplot(KM_fitted)


#looking at high risk group (entering the observation with uptime >=48000h)
longups <- unique(KM_data[KM_data$smart_9_raw>=48000,]$serial_number)
KM_data$longupt <- 0
KM_data[KM_data$serial_number %in% longups,]$longupt <- 1
longrunners <- NROW(unique(KM_data[KM_data$longupt ==1,]$serial_number)) / NROW(unique(KM_data$serial_number))


KM_fitted_hirisk <- survfit(Surv(startdate,date, failure) ~ longupt, data=KM_data)
summary(KM_fitted_hirisk)
autoplot(KM_fitted_hirisk)

#for pbc dataset
data(pbc,package = "survival")

with(pbc, Surv(time,status ==2))
pbc_fitted <- survfit(Surv(time,status ==2)) ~1, data=pbc)
summary(pbc_fitted)
# plotting the graph to show survival rate
autoplot(pbc_fitted)

#COX PH

#Describe Cox, show formulas here:
#Cox function
#Maximum likelihood to estimate betas
#Partial likelihood to estimate betas
#
#


#since we have multiple measurements for each serial we need to specify the start for each measuring period
cox_data <- c
cox_data$startdate <- 0
cox_data[cox_data$date ==1,]$startdate <- 0
cox_data[cox_data$date ==90,]$startdate <- 1
cox_data[cox_data$date ==180,]$startdate <-90
cox_data[cox_data$date ==270,]$startdate <- 180
cox_data[cox_data$date ==366,]$startdate <- 270



#split in train and test
trainsn <- unique(cox_data[cox_data$startdate ==0,]$serial_number)
trainsn <- trainsn[sample(NROW(trainsn))] # shuffle


#Selecting 75% of serial numbers from initial population
sample <- sample.int(n = NROW(trainsn), size = floor(.75*NROW(trainsn)), replace = F)
train <- trainsn[sample ]
test  <- trainsn[-sample ]

trainset <- cox_data[cox_data$serial_number %in% train,]
testset <- cox_data[cox_data$serial_number %in% test,]




#building the cox model

train.cox <- coxph(Surv(startdate, date, failure) ~ smart_9_normalized + smart_5_normalized +smart_187_normalized 
                   +smart_188_normalized+smart_197_normalized + smart_198_normalized,
                   data = trainset)

summary(train.cox)




# predicting hazard ratios on testset (relative to the sample average for all predictor variables) -> these are betas
coxpredicted_train <- predict(train.cox,type="lp")  
coxpredicted_test <- predict(train.cox, newdata=testset,type="lp") 
summary(coxpredicted_train)
summary(coxpredicted_test)

# to get hazard for each subject for a defined day (basehazard*exp(lp))
#cox_bazehaz <- basehaz(train.cox)
#cox_predhaz_t270 <- cox_bazehaz[98,1]*coxpredicted_train

#Calculate Survival probabilities of subjects in testset
#Pred_Prob <- predictSurvProb(train.cox,newdata=testset,times=c(90,180,270,366))  

#Calculating Concordance Index to assess results of prediction
#CI-Formula
#cox implicitly ranks subjects by hazard (thus by survival time)
#goodness of ranking can me measured by CI
Survresptrain <- Surv(trainset$startdate,trainset$date, trainset$failure  ) # calculating baseline hazards
Survresptest <- Surv(testset$startdate,testset$date, testset$failure)

#Cox ranks implicitly the survival times but CI ranks it according to 
CI_cox <- BeggC(Survresptrain, Survresptest, coxpredicted_train, coxpredicted_test)
if(CI_cox<=0.5){
  CI_cox =1-CI_cox
}

# for pbc dataset
data(pbc,package = "survival")
pbc_clean <- pbc[!is.na(pbc$age) & !is.na(pbc$edema) & !is.na(pbc$bili) & !is.na(pbc$albumin) & !is.na(pbc$protime),]  

#Selecting 75%  from initial population
samplepbc <- sample.int(n = NROW(pbc_clean), size = floor(.75*NROW(pbc_clean)), replace = F)
trainpbc <- pbc_clean[samplepbc,]
testpbc  <- pbc_clean[-samplepbc,]
#building the cox model

pbc.cox <- coxph(Surv(time,status ==2 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc)

summary(pbc.cox)

# predicting hazard ratios on testset (relative to the sample average for all predictor variables)
coxpredicted_trainpbc <- predict(pbc.cox,type="lp")  
coxpredicted_testpbc <- predict(pbc.cox, newdata=testpbc,type="lp") 
summary(coxpredicted_trainpbc)
summary(coxpredicted_testpbc)

#Calculating Concordance Index to assess results of prediction

Survresptrainpbc <- Surv(trainpbc$time,trainpbc$status ==2 )
Survresptestpbc <- Surv(testpbc$time,testpbc$status == 2)
CI_coxpbc <- BeggC(Survresptrainpbc, Survresptestpbc, coxpredicted_trainpbc, coxpredicted_testpbc)
if(CI_coxpbc<=0.5){
  CI_coxpbc =1-CI_coxpbc
}

