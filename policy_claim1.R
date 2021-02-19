
#Wczytujemy ramkê danych
policy_data<-read.delim("daneexcel.txt", sep="\t", header=TRUE, stringsAsFactors = FALSE)
#usuwamy kolumnê id i policyno oraz wiek poniewa¿ mamy przedzia³y wiekowe
policy_data<-policy_data[-c(1,6)]


library(stringi)
library(stringr)




#tworzymy funkcjê convert_dates i  konwertujemy daty bez francuskich akcentów
policy_data$PLCYDATE<-lapply(policy_data$PLCYDATE, convert_dates)
#tworzymy funkcjê change dates i zamieniamy francuskie miesi¹ce na liczby w kolumnach z datami
policy_data$PLCYDATE<-lapply(policy_data$PLCYDATE, change_dates)

policy_data$INITDATE<-lapply(policy_data$INITDATE, convert_dates)
policy_data$INITDATE<-lapply(policy_data$INITDATE, change_dates)

policy_data$CLM_DATE<-lapply(policy_data$CLM_DATE,convert_dates)
policy_data$CLM_DATE<-lapply(policy_data$CLM_DATE,change_dates)

policy_data$BIRTH<-lapply(policy_data$BIRTH,convert_dates)
policy_data$BIRTH<-lapply(policy_data$BIRTH,change_dates)


library(stringi)

#konwertujemy z powrotem na character
policy_data$PLCYDATE<-as.character(policy_data$PLCYDATE)

policy_data$INITDATE<-as.character(policy_data$INITDATE)
policy_data$CLM_DATE<-as.character(policy_data$CLM_DATE)
policy_data$BIRTH<-as.character(policy_data$BIRTH)
policy_data$BIRTH<-lapply(policy_data$BIRTH,convert_birth)
policy_data$BIRTH<-as.character(policy_data$BIRTH)


#zamieniamy na typ data

policy_data$PLCYDATE<-as.Date(policy_data$PLCYDATE, format="%d-%m-%y")
policy_data$INITDATE<-as.Date(policy_data$INITDATE, format="%d-%m-%y")
policy_data$CLM_DATE<-as.Date(policy_data$CLM_DATE,format="%d-%m-%y")
policy_data$BIRTH<-as.Date(policy_data$BIRTH,format="%d-%m-%Y")



summary(policy_data)

#zapisujemy do pliku
write.table(policy_data, file = "policy_correct_dates.csv", sep = ",", row.names = FALSE, col.names = TRUE)


