prepare_data2<-read.csv("dane/prepared_data1.csv", header=TRUE, stringsAsFactors = FALSE)


distributions<-read.csv("dane/clear_data.csv", header=TRUE, stringsAsFactors = FALSE)
AGE_CAR<-prepare_data2$AGE_CAR
distributions<-cbind(distributions, AGE_CAR)

write.table(distributions,  file = "dane/original_values.csv", sep=",", row.names = FALSE, col.names = TRUE)

for(var in c("AGE_CAR","RETAINED","YOJ"))
{
  frequency_per_level <- data.frame(table(distributions[,var]))
  colnames(frequency_per_level) <- c("value","n")
  frequency_per_level$value <- as.vector(frequency_per_level$value)
 #frequency_per_level <- rbind(frequency_per_level,data.frame(value = "Not stated",n = length(which(is.na(distributions[,var]) == TRUE)),stringsAsFactors=FALSE))
  frequency_per_level$value <- factor(frequency_per_level$value,levels=c(unique(distributions[,var])[order(unique(distributions[,var]))]))
  
  #print(var)
  #print(frequency_per_level)
  #frequency_per_level$value
  
  print(ggplot(frequency_per_level,
               aes(value,n*100/10296)) +
          geom_bar(stat = "identity",col="black",fill="darkgrey") +
          xlab("") +
          ylab("Percent of records") +
          ggtitle(var) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)))
}

for(var in c("AGE_YEARS","TRAVTIME","BLUEBOOK","HOME_VAL","INCOME","OLDCLAIM","CLM_AMT"))
{
  hist(distributions[distributions[,var] > 0,var],
       xlab="Values",
       ylab="Number of records",
       col="green",
       main=var,
       labels=TRUE)
}

hist(distributions$CLM_AMT[distributions$CLM_AMT >0 & distributions$CLM_AMT < 10000],
     xlab="Values",
     ylab="Number of records",
     main="CLM AMT < $10,000",
     col="green",
     labels=TRUE)

write.table(prepare_data2, file = "dane/prepared_data2.csv", sep = ",", row.names = FALSE, col.names = TRUE)

