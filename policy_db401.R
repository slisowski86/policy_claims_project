policy_db5<-read.csv("policy_before_na.csv", sep=",", header=TRUE)

# w kolumnie JOBCLASS warto?? niezdefiniowanego zawodu zast?pujemy warto?ci? NA



summary(policy_db5)



#we wszystkich kolumnach warto?ci NA zast?pujemy ?rednimi

names<-c("YOJ", "INCOME", "JOBCLASS", "HOME_VAL", "SAMEHOME")

to_change<-col_to_change(policy_db5, names)
to_change



policy_db5[to_change]<-lapply(policy_db5[to_change],  na_replace 
)

summary(policy_db5)

write.table(policy_db5, file = "policy_after_clear.csv", sep = ",", row.names = FALSE, col.names = TRUE)