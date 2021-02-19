prepare_data5<-read.csv("dane/prepared_data5.csv", header = TRUE, stringsAsFactors = FALSE)

print("Crash rate when CAR_AGE = 1:")
round((length(which(prepare_data5$CAR_AGE == 1 & prepare_data5$CLM_FLAG == 1))*100)/length(which(prepare_data5$CAR_AGE == 1)),digits=2)
print("Crash rate when CAR_AGE >= 20:")
round((length(which(prepare_data5$CAR_AGE >= 20 & prepare_data5$CLM_FLAG == 1))*100)/length(which(prepare_data5$CAR_AGE >= 20)),digits=2)

count_per_target_and_predictor <- prepare_data5[prepare_data5$CAR_AGE >= 3 & prepare_data5$CAR_AGE <= 19 & is.na(prepare_data5$CAR_AGE) == FALSE,c("CAR_AGE","CLM_FLAG")] %>%
  count(CLM_FLAG,CAR_AGE)	

count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- prepare_data5[prepare_data5$CAR_AGE >= 3 & prepare_data5$CAR_AGE <= 19 & is.na(prepare_data5$CAR_AGE) == FALSE,c("CAR_AGE","CLM_FLAG")] %>%
  count(CAR_AGE)

count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by="CAR_AGE")

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$CLM_FLAG == 1,]

count_per_target_and_predictor <- data.frame(value = count_per_target_and_predictor$CAR_AGE,
                                             crash.rate = count_per_target_and_predictor$n.x*100/count_per_target_and_predictor$n.y)

count_per_target_and_predictor$value <- factor(count_per_target_and_predictor$value,levels=3:19)

ggplot(count_per_target_and_predictor,
       aes(value,crash.rate)) +
  geom_bar(stat = "identity") +
  xlab("CAR_AGE") +
  ylab("CLM_FLAG_YES(%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----RETAINED-vs-target-flag, echo=FALSE, eval=TRUE---------------------------
RETAINED_for_barplot <- prepare_data5[prepare_data5$RETAINED == 1 | prepare_data5$RETAINED == 3 | prepare_data5$RETAINED == 4 | prepare_data5$RETAINED == 6 | prepare_data5$RETAINED == 7 | prepare_data5$RETAINED == 9 | prepare_data5$RETAINED >= 10,c("CLM_FLAG","RETAINED")]

RETAINED_for_barplot$RETAINED <- ifelse(RETAINED_for_barplot$RETAINED > 10,"11+",RETAINED_for_barplot$RETAINED)

count_per_target_and_predictor <- RETAINED_for_barplot %>% count(CLM_FLAG,RETAINED)
count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- RETAINED_for_barplot %>% count(RETAINED)
count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by="RETAINED")

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$CLM_FLAG == 1,]

count_per_target_and_predictor <- data.frame(value = count_per_target_and_predictor$RETAINED,
                                             crash.rate = count_per_target_and_predictor$n.x*100/count_per_target_and_predictor$n.y)

count_per_target_and_predictor$value <- factor(count_per_target_and_predictor$value,levels=c("1","3","4","6","7","9","10","11+"))

ggplot(count_per_target_and_predictor,
       aes(value,crash.rate)) +
  geom_bar(stat = "identity") +
  xlab("RETAINED") +
  ylab("CLM_FLAG_YES(%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----mvr-pts-vs-target-flag, echo=FALSE, eval=TRUE-----------------------
count_per_target_and_predictor <- prepare_data5[,c("CLM_FLAG","MVR_PTS")] %>% count(CLM_FLAG,MVR_PTS)
count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- prepare_data5[,c("CLM_FLAG","MVR_PTS")] %>% count(MVR_PTS)
count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by="MVR_PTS")

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$CLM_FLAG == 1,]

count_per_target_and_predictor <- data.frame(value = count_per_target_and_predictor$MVR_PTS,
                                             crash.rate = count_per_target_and_predictor$n.x*100/count_per_target_and_predictor$n.y)

count_per_target_and_predictor$value <- factor(count_per_target_and_predictor$value,levels=0:10)

ggplot(count_per_target_and_predictor,
       aes(value,crash.rate)) +
  geom_bar(stat = "identity") +
  xlab("MVR_PTS") +
  ylab("CLM_FLAG_YES") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

par(mfrow=c(1,2))

plot(as.numeric(as.vector(count_per_target_and_predictor$value)),
     count_per_target_and_predictor$crash.rate,
     xlab="Motor vehicle record points (raw)",
     ylab="CLM_FLAG_YES (%)",
     type="o")

abline(lm(count_per_target_and_predictor$crash.rate ~ as.numeric(as.vector(count_per_target_and_predictor$value))),lty=2)

plot(ifelse(as.numeric(as.vector(count_per_target_and_predictor$value)) >= 7,as.numeric(as.vector(count_per_target_and_predictor$value)) + 3,as.numeric(as.vector(count_per_target_and_predictor$value))),
     count_per_target_and_predictor$crash.rate,
     xlab="Motor vehicle record points (modified)",
     ylab="CLM_FLAG_YES(%)",
     main="Add 3 when points >= 7",
     type="o")

abline(lm(count_per_target_and_predictor$crash.rate ~ ifelse(as.numeric(as.vector(count_per_target_and_predictor$value)) >= 7,as.numeric(as.vector(count_per_target_and_predictor$value)) + 3,as.numeric(as.vector(count_per_target_and_predictor$value)))),lty=2)
