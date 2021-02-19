prepare_data6<-read.csv("dane/prepared_data5.csv", header = TRUE, stringsAsFactors = FALSE)

## ----cor-travtime-bluebook-income-target-flag, echo=FALSE, eval=TRUE-----
par(mfrow=c(1,2))
boxplot(TRAVTIME ~ factor(CLM_FLAG),data=prepare_data6,ylab="TRAVTIME")
boxplot(TRAVTIME ~ factor(CLM_FLAG),data=prepare_data6[prepare_data6$TRAVTIME < 60,],ylab="TRAVTIME",main="TRAVTIME < 60 only")

par(mfrow=c(1,2))
boxplot(INCOME ~ factor(CLM_FLAG),data=prepare_data6,ylab="INCOME")
boxplot(BLUEBOOK ~ factor(CLM_FLAG),data=prepare_data6,ylab="BLUEBOOK")
boxplot(log10(INCOME + 1)  ~ factor(CLM_FLAG),data=prepare_data6,ylab="log10(INCOME + 1)")
boxplot(log10(BLUEBOOK) ~ factor(CLM_FLAG),data=prepare_data6,ylab="log10(BLUEBOOK)")

