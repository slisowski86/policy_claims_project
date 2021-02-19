prepare_data6<-read.csv("dane/data_transformed.csv", header=TRUE, stringsAsFactors = FALSE)

names<-c("CAR_AGE", "AGE_CAR", "NEW_IN_JOB")
to_delete<-col_to_change(prepare_data6, names)

prepare_data6<-prepare_data6[-to_delete]

colnames(prepare_data6) <- plyr::mapvalues(colnames(prepare_data6),from="YOJ",to="NEW_IN_JOB")


prepare_data6[,"NEW_IN_JOB"] <- ifelse(prepare_data6[,"NEW_IN_JOB"] == 0,1,0)


write.table(prepare_data6, file = "dane/data_transformed2.csv", sep = ",", row.names = FALSE, col.names = TRUE)
