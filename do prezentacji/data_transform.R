data_trans<-read.csv("dane/prepared_data5.csv", header = TRUE, stringsAsFactors = FALSE)

add_age<-read.csv("dane/original_values.csv", header = TRUE, stringsAsFactors = FALSE)

AGE_YEARS<-add_age$AGE_YEARS
data_trans<-cbind(data_trans, AGE_YEARS)
data_trans <- data.frame(data_trans,
                        College_MAX_EDUC = ifelse(data_trans$MAX_EDUC == "Bachelors" | data_trans$MAX_EDUC == "Masters" | data_trans$MAX_EDUC == "PhD",1,0),
                        Sedan = ifelse(data_trans$CAR_TYPE  == "Sedan",1,0),
                        Student = ifelse(data_trans$JOBCLASS == "Student",1,0),
                        Blue_collar = ifelse(data_trans$JOBCLASS == "Blue Collar",1,0),
                        check.names=FALSE,stringsAsFactors=FALSE)

age_groups <- ifelse(data_trans$AGE_YEARS < 25,"M³odzi (<25)","Pomiêdzy (25-64)")
age_groups <- ifelse(data_trans$AGE_YEARS >= 65,"Seniorzy (65+)",age_groups)

for(var in c("M³odzi (<25)","Pomiêdzy (25-64)","Seniorzy (65+)"))
{
  crash_rate <- round((length(which(age_groups == var & data_trans$CLM_FLAG == 1))*100)/length(which(age_groups == var)),digits=2)
  print(paste0("Procent roszczeñ dla grupy ",var,": ",crash_rate))
}

#Zmienn¹ AGE zamieniamy na binarn¹

data_trans <- data.frame(data_trans,
                        Young_age = ifelse(data_trans$AGE_YEARS < 25,1,0),
                        check.names=FALSE,stringsAsFactors=FALSE)
#Dodajemy zmienn¹ binarn¹ homeowner

data_trans <- data.frame(data_trans,
                        Homeowner = ifelse(data_trans$HOME_VAL > 0,1,0),
                        stringsAsFactors=FALSE,check.names=FALSE)

write.table(data_trans, file = "dane/data_transformed.csv", sep = ",", row.names = FALSE, col.names = TRUE)
