policy_db3<-read.csv("policy_correct_dates.csv", header=TRUE)

library(BBmisc)
library(stringr)
library(searchable)

names<-c("BLUEBOOK", "OLDCLAIM", "CLM_AMT","INCOME", "HOME_VAL")


to_change<-c()
for( i in seq_along(colnames(policy_db3))){
  for(j in seq_along(names)){
    if(names[j]==colnames(policy_db3[i])){
      n<-match(names[j], names(policy_db3))
      to_change<-c(to_change,n)
    }
  }
}



policy_db3[to_change]<-lapply(policy_db3[to_change], function(x)
  x<-as.numeric(gsub("[\\$]", "", x))
            
)

policy_db3[to_change]<-lapply(policy_db3[to_change],norm_func)



policy_db3$PLCYDATE<-as.Date.character(policy_db3$PLCYDATE, format="%Y-%m-%d")
policy_db3$BIRTH<-as.Date.character(policy_db3$BIRTH, format="%Y-%m-%d")

#dodanie kolumny wiek do ramki danych


age_years<-c(as.numeric(difftime(policy_db3$PLCYDATE, policy_db3$BIRTH, units="weeks")))
age_years<-age_years/52
age_years<-round(age_years, digits=0)
age_years

policy_db3<-cbind(policy_db3, age_years)
colnames(policy_db3)[which(colnames(policy_db3)=="age_years")]<-"AGE_YEARS"

#usuniêcie kolumn CLM_AMT i CLM_DATE
x<-which(colnames(policy_db3)=="CLM_AMT")
y<-which(colnames(policy_db3)=="CLM_DATE")

policy_db3<-policy_db3[-c(x,y)]

#zamiana kolumn z wartoœciami YEs NO na wartoœci binarne

names_binary<-c("RED_CAR", "REVOKED", "CLM_FLAG","MARRIED", "PARENT1")


to_change_binary<-c()
for( i in seq_along(colnames(policy_db3))){
  for(j in seq_along(names_binary)){
    if(names_binary[j]==colnames(policy_db3[i])){
      n<-match(names_binary[j], names(policy_db3))
      to_change_binary<-c(to_change_binary,n)
    }
  }
}

to_change_binary

policy_db3[to_change_binary]<-lapply(policy_db3[to_change_binary], replace_binary)


write.table(policy_db3, file = "policy_integer_binary.csv", sep = ",", row.names = FALSE, col.names = TRUE)
