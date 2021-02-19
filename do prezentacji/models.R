#zaczynamy modelowanie korzystamy z rozk³adu poissona
library(MXM)
library(caret)
library(boot)
library(pscl)
library(MASS)
library(foreign)
library(pROC)
library(randomForest)
library(ROCR)
library("OptimalCutpoints")
library(stats)
library(AICcmodavg)
library(cvAUC)
library(binaryLogic)
data(ROCR.simple)
class(ROCR.simple$predictions)

model_data<-read.csv("dane/data_transformed_all.csv", header=TRUE, stringsAsFactors = FALSE)
summary(model_data)

model_data_train<-data.frame(model_data[1:6999,])
model_data_test<-data.frame(model_data[7000:10296,])
model_1<-glm(CLM_FLAG ~	Driving_kids+Past_claim+REVOKED+NEW_IN_JOB+
               Young_age+College_MAX_EDUC+Sedan+URBAN_NOT_RURAL, data=model_data_train,
             family=poisson(link="logit"))
summary(model_1)

model_data_test$CLM_FLAG<-as.numeric(model_data_test$CLM_FLAG)

model1_test<-glm(CLM_FLAG ~	Driving_kids+TRAVTIME+Past_claim+REVOKED+MVR_PTS+NEW_IN_JOB+
                   Young_age+College_MAX_EDUC+marriage_home_and_kids_score+Sedan+URBAN_NOT_RURAL, data=model_data_test,
                 family=poisson(link="logit"))
summary(model1_test)


check<-varImp(model_1, scale=FALSE)
check






model_2<-glm(CLM_FLAG ~	Driving_kids+TRAVTIME+Past_claim+REVOKED+MVR_PTS+NEW_IN_JOB+
               Young_age+College_MAX_EDUC+marriage_home_and_kids_score+Sedan+URBAN_NOT_RURAL, 
             family=binomial, data=model_data_train)

summary(model_2)

summary(model_2)
model2_test<-glm(CLM_FLAG ~	Driving_kids+TRAVTIME+Past_claim+REVOKED+MVR_PTS+NEW_IN_JOB+
                        Young_age+College_MAX_EDUC+marriage_home_and_kids_score+Sedan+URBAN_NOT_RURAL, 
                      family=binomial(link="logit"), data=model_data_test)

summary(model2_test)




check2<-varImp(model_2, scale=FALSE)
check2


# Rozk³ad dwumianowy ujemny

model_3<-glm.nb(CLM_FLAG ~Driving_kids+TRAVTIME+Past_claim+REVOKED+MVR_PTS+NEW_IN_JOB+
                  Young_age+College_MAX_EDUC+marriage_home_and_kids_score+Sedan+URBAN_NOT_RURAL, 
             data=model_data_train)


summary(model_3)
check3<-varImp(model_3, scale=FALSE)
check3

model3_test<-glm.nb(CLM_FLAG ~	Driving_kids+TRAVTIME+Past_claim+REVOKED+MVR_PTS+NEW_IN_JOB+
         Young_age+College_MAX_EDUC+marriage_home_and_kids_score+Sedan+URBAN_NOT_RURAL, 
       data=model_data_test)


#porównujemy modele

model_1_out <- cbind(AIC=AIC(model_1),  BIC = BIC(model_1), loglik=logLik(model_1))

model_2_out <- cbind(AIC=AIC(model_2),  BIC = BIC(model_2), loglik=logLik(model_2))
model_3_out <- cbind(AIC=AIC(model_3),  BIC = BIC(model_3), loglik=logLik(model_3))
model_comp <- rbind(model_1_out,  model_2_out , model_3_out)
rownames(model_comp) <- c("model_1","model_2","model_3")
model_comp
#ewaluacja modeli

policy_predict1<-predict(model1_test, data=model_data_test, type="response")

true_clm_flag<-as.numeric(model_data_test$CLM_FLAG)

policy_predict1<-as.numeric(policy_predict1)

typeof(auc_sample1$policy_predict1)
auc_sample1<-list(policy_predict1,true_clm_flag)
names(auc_sample1)<-c("predictions", "labels")
auc_sample1
AUC(auc_sample1$predictions, auc_sample1$labels, label.ordering = NULL)
policy_predict1<-ifelse(policy_predict1 > 0.5,1,0)
roc(predictor = policy_predict1,
    response = model_data_test$CLM_FLAG,
    
    main="Regresja Poissona",
    plot=TRUE)

model_data_test$CLM_FLAG<-as.factor(model_data_test$CLM_FLAG)
policy_predict1<-as.factor(policy_predict1)
caret::confusionMatrix(data=policy_predict1,
                       reference=model_data_test$CLM_FLAG,
                       positive="1")

pref_df1<-data.frame(pred=policy_predict1, truth=model_data_test$CLM_FLAG)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df1, tag.healthy = "0")
summary(oc)
policy_predict2<-predict(object=model2_test, data=model_data_test, type="response")
roc(predictor = policy_predict2,
    response = model_data_test$CLM_FLAG,
    
    main="Regresja dwumianowa",
    plot=TRUE)

model_data_test$CLM_FLAG<-as.factor(model_data_test$CLM_FLAG)
caret::confusionMatrix(data=policy_predict2,
                       reference=model_data_test$CLM_FLAG,
                       positive="1")

policy_predict2<- predict(model2_test, model_data_test, type="response")
pref_df2<-data.frame(pred=policy_predict2, truth=model_data_test$CLM_FLAG)
oc <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_df2, tag.healthy = "0")
summary(oc)
auc_sample2<-list(policy_predict2, true_clm_flag)

names(auc_sample2)<-c("predictions", "labels")
AUC(auc_sample2$predictions, auc_sample2$labels, label.ordering = NULL)
policy_predict3<-predict(object=model3_test, data=model_data_test, type="response")

policy_predict3<-ifelse(policy_predict3 > 0.5,1,0)
policy_predict3<-as.factor(policy_predict3)
model_data_test$CLM_FLAG<-as.factor(model_data_test$CLM_FLAG)
caret::confusionMatrix(data=policy_predict3,
                       reference=model_data_test$CLM_FLAG,
                       positive="1")

policy_predict3<-as.numeric(policy_predict3)
auc_sample3<-list(policy_predict3, true_clm_flag)

names(auc_sample3)<-c("predictions", "labels")
AUC(auc_sample3$predictions, auc_sample3$labels, label.ordering = NULL)
roc(predictor = policy_predict3,
     response = model_data_test$CLM_FLAG,
     main="Regresja Dwumianowa ujemna",
     plot=TRUE)
AIC(model_1)
BIC(model_1)



