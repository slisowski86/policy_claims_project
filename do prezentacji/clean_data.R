#³adujemy potrzebne biblioteki
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(caret)
library(pROC)

#³adujemy oczyszczony plik
policy_clean<-read.csv("dane/clear_data.csv", sep=",", header=TRUE)
summary(policy_clean)
policy_clean2<-policy_clean
#sprawdzamy ile jest unikalnych wartoœci w ka¿dej kolumnie

policy_clean$MVR_PTS<-as.factor(policy_clean$MVR_PTS)

#Zmienne ci¹g³e TRAVTIME, BLUEBOOK, OLDCLAIM, HOME_VAL, INCOME zmieniamy na zmienne kategorycznedziel¹c je na przedzia³y 

summary(policy_clean)

#zmienna TRAVTIME bêdzie mia³a przedzia³y 0-40, 41-80, 81-120, 121-160

cat_travtime<-cut(policy_clean$TRAVTIME, breaks=c(0,40,80,120,160), labels=c("0-40", "40-80", "80-120", "120-160"), right=TRUE)
policy_clean$TRAVTIME<-cat_travtime


hist(policy_clean$BLUEBOOK)
#zmienna BLUEBOOK bêdzie mia³a przedzia³y 0-20000, 20001-40000, 40001-60000, 60001-80000
cat_bluebook<-cut(policy_clean$BLUEBOOK, breaks=c(0,20000,40000,60000,80000), labels=c("0-200", "200-400", "400-600", "600-800"), right=FALSE)
policy_clean$BLUEBOOK<-cat_bluebook
summary(policy_clean$OLDCLAIM)
cat_oldclaim<-cut(policy_clean$OLDCLAIM, breaks=c(0,15000,30000,45000,60000), labels=c("0-15", "15-30", "30-45", "45-60"), right=FALSE)
policy_clean$OLDCLAIM<- cat_oldclaim
summary(policy_clean$BLUEBOOK)

hist(policy_clean$CLM_AMT)
cat_clmamt<-cut(policy_clean$CLM_AMT, breaks=c(0,50000,100000,150000),labels=c("0-50", "50-100", "100-150"), right=FALSE)
table(cat_clmamt)
policy_clean$CLM_AMT<-cat_clmamt
table(cat_clmamt)

cat_yoj<-cut(policy_clean$YOJ, breaks=c(0,6,12,18,24),labels=c("0-6", "6-12", "12-18", "18-24"), right=FALSE)
cat_yoj
policy_clean$YOJ<-cat_yoj
table(cat_yoj)
summary(policy_clean$YOJ)

table(policy_clean$YOJ)

summary(policy_clean$INCOME)

cat_income<-cut(policy_clean$INCOME, breaks=c(0,100000,200000,300000,400000),labels=c("0-100", "100-200", "200-300", "300-400"), right=FALSE)

policy_clean$INCOME<-cat_income

summary(policy_clean$HOME_VAL)

cat_homeval<-cut(policy_clean$HOME_VAL, breaks=c(0,250000,500000,750000,1000000),labels=c("0-250", "250-500", "500-750", "750-1000"), right=FALSE)
table(cat_homeval)
policy_clean$HOME_VAL<-cat_homeval

summary(policy_clean$SAMEHOME)

n<-which(policy_clean$SAMEHOME=="-3")
n
policy_clean$SAMEHOME[6662]<-0
summary(policy_clean$SAMEHOME)
cat_samehome<-cut(policy_clean$SAMEHOME, breaks=c(0,8,16,24,32),labels=c("0-8", "8-16", "16-24", "24-32"), right=FALSE)
table(cat_samehome)
policy_clean$SAMEHOME<-cat_samehome

cat_retained<-cut(policy_clean$RETAINED, breaks=c(0,7,14,21,28),labels=c("0-7", "7-14", "14-21", "21-28"), right=FALSE)
summary(cat_retained)
policy_clean$RETAINED<-cat_retained
write.table(policy_clean, file = "dane/policy_factor.csv", sep = ",", row.names = FALSE, col.names = TRUE)

policy_clean2$HOME_VAL[6236]
