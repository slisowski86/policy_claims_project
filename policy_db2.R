#wczytujemy plik z poprawnymi datami
policy_data2<-read.csv("dane/policy_correct_dates.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
summary(policy_data2)


#uzuwamy znaki dolara z komórek z liczbami
#tworzymy macierz do funkcji apply

policy_data2$BLUEBOOK<-sapply(policy_data2$BLUEBOOK, function(x){
  x<-as.numeric(gsub("[\\$]", "", x))})
policy_data2$OLDCLAIM<-sapply(policy_data2$OLDCLAIM, function(x){
  x<-as.numeric(gsub("[\\$]", "", x))})
policy_data2$CLM_AMT<-sapply(policy_data2$CLM_AMT, function(x){
  x<-as.numeric(gsub("[\\$]", "", x))})
policy_data2$INCOME<-sapply(policy_data2$INCOME, function(x){
  x<-as.numeric(gsub("[\\$]", "", x))})
policy_data2$HOME_VAL<-sapply(policy_data2$HOME_VAL, function(x){
  x<-as.numeric(gsub("[\\$]", "", x))})

table(policy_data2$MARRIED)



