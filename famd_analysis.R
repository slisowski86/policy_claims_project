policy_mix<-read.csv("dane/data_to_famd.csv", sep=",", header = TRUE)

#dzielimy dane na ci¹g³e i kategoryczne do danych kategorycznych zaliczmy te¿ zmienne dyskretne
summary(policy_mix)


plot(policy_mix$CLM_FLAG, policy_mix$CAR_TYPE)

names_cat<-c( "CAR_USE", "CAR_TYPE", "RED_CAR","REVOKED", "AGE", "AGE.GENDER", "GENDER",
             "MARRIED", "PARENT1", "JOBCLASS", "MAX_EDUC", "DENSITY")

cat_var<-c(col_to_change(policy_mix, names_cat))
cat_var_df<-policy_mix[cat_var]

int_var_df<-policy_mix[-cat_var]
int_var_df<-int_var_df[-(which(colnames(int_var_df)=="CLM_FLAG"))]

#standaryzujemy zmienne liczbowe
library(BBmisc)

int_var_df[1:15]<-lapply(int_var_df[1:15], stand_function)
int_var_df[1:15]<-lapply(int_var_df[1:15], function(x)
  x<-round(x, digits=4)
  )

summary(int_var_df)

#1 u¿ywamy funkcji FAMD 
library(FactoMineR)
mix_data<-FAMD(policy_mix[-(which(colnames(int_var_df)=="CLM_FLAG"))])
summary(mix_data$quanti.var)

summary(cat_var_df)

levels(cat_var_df$CAR_TYPE)


pickup<-c(which(policy_mix$CLM_FLAG=="Yes"))

library(plyr)
counts <- ddply(policy_mix, .(policy_mix$CAR_TYPE, policy_mix$CLM_FLAG), nrow)
names(counts) <- c("y", "m", "Freq")

counts
