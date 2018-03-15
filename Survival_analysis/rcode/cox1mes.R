
memory.limit(30000)
if(!require("survival")) install.packages("survival"); library("survival")
if(!require("survminer")) install.packages("survminer"); library("survminer")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("ggfortify")) install.packages("ggfortify"); library("ggfortify")
if(!require("survAUC")) install.packages("survAUC"); library("survAUC")
if(!require("pec")) install.packages("pec"); library("pec")
if(!require("timereg")) install.packages("timereg"); library("timereg")
if(!require("KMsurv")) install.packages("KMsurv"); library("KMsurv")

#c <- readRDS("./Survival_analysis/data_2016_rds/hdd_onemeas.rds")# remove startdate from Survobjects when using this!
c <- readRDS("./Survival_analysis/data_2016_rds/hdd_Dataset.rds")
#Kaplan Meier
KM_data <- c


#creating the survival object
with(KM_data, Surv(date, failure))
KM_fitted <- survfit(Surv(date, failure) ~ 1, data=KM_data)
summary(KM_fitted)
autoplot(KM_fitted_hirisk)


#looking at high risk group (entering the observation with uptime >=48000h)
longups <- unique(KM_data[KM_data$smart_9_raw>=48000,]$serial_number)
KM_data$longupt <- 0
KM_data[KM_data$serial_number %in% longups,]$longupt <- 1
longrunners <- NROW(unique(KM_data[KM_data$longupt ==1,]$serial_number)) / NROW(unique(KM_data$serial_number))


KM_fitted_hirisk <- survfit(Surv(date, failure) ~ longupt, data=KM_data)
summary(KM_fitted_hirisk)
autoplot(KM_fitted_hirisk)


#COX PH

#Describe Cox, show formulas here:
#Cox function
#Maximum likelihood to estimate betas
#Partial likelihood to estimate betas
#
#


#since we don't have multiple measurements here
cox_data <- c




#split in train and test
trainsn <- unique(cox_data$serial_number)
trainsn <- trainsn[sample(NROW(trainsn))] # shuffle


#Selecting 75% of serial numbers from initial population
sample <- sample.int(n = NROW(trainsn), size = floor(.75*NROW(trainsn)), replace = F)
train <- trainsn[sample ]
test  <- trainsn[-sample ]

trainset <- cox_data[cox_data$serial_number %in% train,]
testset <- cox_data[cox_data$serial_number %in% test,]




#building the cox model

train.cox <- coxph(Surv(date, failure,type ='right') ~ smart_9_normalized + smart_5_normalized +smart_187_normalized 
                   +smart_188_normalized+smart_197_normalized + smart_198_normalized,
                   data = trainset,model=TRUE)

summary(train.cox)




# predicting hazard ratios on testset (relative to the sample average for all predictor variables)
coxpredicted_train <- predict(train.cox,type="lp")  
coxpredicted_test <- predict(train.cox, newdata=testset,type="lp") 
summary(coxpredicted_train)
summary(coxpredicted_test)

# to get hazard for each subject for a defined day (basehazard*exp(lp))
#cox_bazehaz <- basehaz(train.cox)
#cox_predhaz_t90 <- cox_bazehaz[2,1]*coxpredicted_train

#Calculate Survival probabilities of subjects in testset
#Pred_Prob <- predictSurvProb(train.cox,newdata=testset,times=c(90,180,270,366))  

#Calculating Concordance Index to assess results of prediction
#CI-Formula
#cox implicitly ranks subjects by hazard (thus by survival time)
#goodness of ranking can me measured by CI
Survresptrain <- Surv(trainset$date, trainset$failure,type ='right'  )
Survresptest <- Surv(testset$date, testset$failure,type ='right')
CI_cox <- BeggC(Survresptrain, Survresptest, coxpredicted_train, coxpredicted_test)
if(CI_cox<=0.5){
  CI_cox =1-CI_cox
}

