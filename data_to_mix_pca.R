policy_famd<-read.csv("dane/policy_correct_dates.csv", header=TRUE)

#usuwamy znak dolara ze zmiennych u¿ywamy stworzonej funkcji col_to_change i remove dollar

names<-c("BLUEBOOK", "OLDCLAIM", "CLM_AMT","INCOME", "HOME_VAL")

to_change<-c()

to_change<-col_to_change(policy_famd, names)

policy_famd[to_change]<-lapply(policy_famd[to_change], remove_dollar)

#tworzymy osobn¹ kolumnê wiek odejmujemy od kolumny PLCYDATE kolumnê BIRTH

policy_famd$PLCYDATE<-as.Date.character(policy_famd$PLCYDATE, format="%Y-%m-%d")
policy_famd$BIRTH<-as.Date.character(policy_famd$BIRTH, format="%Y-%m-%d") 

age_years<-c(as.numeric(difftime(policy_famd$PLCYDATE, policy_famd$BIRTH, units="weeks")))
age_years<-age_years/52
age_years<-round(age_years, digits=0)

policy_famd<-cbind(policy_famd, age_years)
colnames(policy_famd)[which(colnames(policy_famd)=="age_years")]<-"AGE_YEARS"

#usuwamy kolumny z datami PLCYDATE, INITDATE. CLM_DATE, BIRTH

names_date<-c("PLCYDATE", "INITDATE", "CLM_DATE", "BIRTH")

to_change_dates<-col_to_change(policy_famd, names_date)

policy_famd<-policy_famd[-to_change_dates]

#usuwamy kolumnê YEARQTR

y<-which(colnames(policy_famd)=="YEARQTR")
policy_famd<-policy_famd[-y]
summary(policy_famd)

#zatêpujemy wartoœci NA w kolumnach YOJ, INCOME, HOME_VAL, SAMEHOME waroœciami œrednimi funkcja
#na_replace


names_with_na<-c("YOJ", "INCOME", "HOME_VAL", "SAMEHOME")
to_remove_na<-col_to_change(policy_famd, names_with_na)

policy_famd[to_remove_na]<-lapply(policy_famd[to_remove_na], na_replace)
policy_famd[to_remove_na]<-lapply(policy_famd[to_remove_na], function(x)
  x<- round(x, digits=0))

summary(policy_famd)

write.table(policy_famd, file = "dane/data_to_famd.csv", sep = ",", row.names = FALSE, col.names = TRUE)
