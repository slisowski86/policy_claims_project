policy_db4<- read.csv("dane/policy_integer_binary.csv", sep=",",  header = TRUE)

names<-c("BLUEBOOK", "OLDCLAIM", "CLM_AMT","INCOME", "HOME_VAL")


to_change<-c()
for( i in seq_along(colnames(policy_db4))){
  
  for(j in seq_along(names)){
    if(names[j]==colnames(policy_db4[i])){
      n<-match(names[j], names(policy_db4))
      to_change<-c(to_change,n)
    }
  }
}


policy_db4[to_change]<-lapply(policy_db4[to_change], function(x)
  x<-round(x, digits=4))

#usuwamy kolumnê YEARQTR, AGE, AGE_GENDER,
d<-which(colnames(policy_db4)=="YEARQTR")

a<-which(colnames(policy_db4)=="AGE")
ag<-which(colnames(policy_db4)=="AGE.GENDER")

policy_db4<-policy_db4[-c(d,a,ag)]

w<-normalize_func(policy_db4$AGE_YEARS)
w
#normalizujemny zmienn¹ wiek

library(BBmisc)

new_age<-normalize_func(policy_db4$AGE_YEARS)
policy_db4$AGE_YEARS<-new_age

summary(policy_db4)

#normalizujemy pozosta³e zmienne liczbowe

names_norm<-c("TRAVTIME", "RETAINED",  "CLM_FREQ", "MVR_PTS", "HOMEKIDS", "YOJ", "SAMEHOME", "NPOLICY" )

to_normalize<-c()
for( i in seq_along(colnames(policy_db4))){
  
  for(j in seq_along(names_norm)){
    if(names_norm[j]==colnames(policy_db4[i])){
      n<-match(names_norm[j], names(policy_db4))
      to_normalize<-c(to_normalize,n)
    }
  }
}

policy_db4[to_normalize]<-lapply(policy_db4[to_normalize], norm_func)
policy_db4[to_normalize]<-lapply(policy_db4[to_normalize], function(x)
                                 round(x, digits=4)
                                 )

#tworzymy kolumnê okres posiadania polis


policy_db4$PLCYDATE<-as.Date.character(policy_db4$PLCYDATE, format="%Y-%m-%d")
policy_db4$INITDATE<-as.Date.character(policy_db4$INITDATE, format="%Y-%m-%d") 

plcy_years<-c(as.numeric(difftime(policy_db4$PLCYDATE, policy_db4$INITDATE, units="weeks")))
plcy_years<-plcy_years/52
plcy_years<-round(plcy_years, digits=0)


policy_db4<-cbind(policy_db4, plcy_years)
colnames(policy_db4)[which(colnames(policy_db4)=="plcy_years")]<-"PLCY_YEARS"
summary(policy_db4$PLCY_YEARS)


# normalizujemy stworzon¹ kolumnê

#policy_db4$PLCY_YEARS<-normalize(policy_db4$PLCY_YEARS,  method="range",range=c(0,1), margin = 1L, on.constant = "quiet")

#summary(policy_db4)

#usuwamy kolumny z datami



pd<-which(colnames(policy_db4)=="PLCYDATE")
id<-which(colnames(policy_db4)=="INITDATE")
b<-which(colnames(policy_db4)=="BIRTH")

policy_db4<-policy_db4[-c(pd,id,b)]

#zmienn¹ p³eæ i sposób u¿ytkowania samochodu zamieniamy na binarn¹ 

policy_db4$GENDER<-replace_binary(policy_db4$GENDER)
policy_db4$CAR_USE<-replace_binary(policy_db4$CAR_USE)

#normalizujemy zmienn¹ KIDS_DRIV

policy_db4$KIDSDRIV<-normalize(policy_db4$KIDSDRIV,  method="range",range=c(0,1), margin = 1L, on.constant = "quiet")


#zmienne CAR_TYPE, JOBCLASS, MAX_EDUC, DENSITY zamieniamy na zmienne numeryczne ze wzglêdu na dany poziom funkcja col_to_change

names_lev<-c("CAR_TYPE", "JOBCLASS", "MAX_EDUC", "DENSITY")

change<-col_to_change(policy_db4,names_lev)


policy_db4[change]<-lapply(policy_db4[change], replace_levels)

#zmieniamy factory na zmienne numeryczne

policy_db4$CAR_TYPE<-as.numeric(paste(policy_db4$CAR_TYPE))
policy_db4$JOBCLASS<-as.numeric(paste(policy_db4$JOBCLASS))
policy_db4$MAX_EDUC<-as.numeric(paste(policy_db4$MAX_EDUC))
policy_db4$DENSITY<-as.numeric(paste(policy_db4$DENSITY))
policy_db4$CAR_USE<-as.numeric(paste(policy_db4$CAR_USE))
policy_db4$GENDER<-as.numeric(paste(policy_db4$GENDER))

summary(policy_db4)

policy_db4$AGE_YEARS<-round(policy_db4$AGE_YEARS, digits = 4)

#zapisujemy

write.table(policy_db4, file = "policy_before_na.csv", sep = ",", row.names = FALSE, col.names = TRUE)
