policy_nc<- read.csv("dane/policy_correct_dates.csv", sep=",",  header = TRUE)

#usuwamy kolumn? YEARQTR

y<-which(colnames(policy_nc)=="YEARQTR")
policy_nc<-policy_nc[-y]

#usuwamy znaki dolara z kolumn z warto?ciami funkcja col_to_change
names<-c("BLUEBOOK", "OLDCLAIM", "CLM_AMT","INCOME", "HOME_VAL")

to_change<-col_to_change(policy_nc, names)

policy_nc[to_change]<-lapply(policy_nc[to_change], remove_dollar)

summary(policy_nc)

