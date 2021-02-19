# Zmieniamy
prepared_data3<-read.csv("dane/prepared_data2.csv", header = TRUE, stringsAsFactors = FALSE)

distributions<-read.csv("dane/original_values.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(prepared_data3) <- plyr::mapvalues(colnames(prepared_data3),
                                       from=c("CLM_FREQ","HOMEKIDS","KIDSDRIV"),
                                       to=c("Past_claim","Kids","Driving_kids"))

for(var in c("Past_claim","Kids","Driving_kids"))
{
  prepared_data3[,var] <- ifelse(prepared_data3[,var] > 0,1,0)
  
}



#konwerujemy zmienn¹ YOJ na zmienn¹ binarn¹

colnames(distributions) <- plyr::mapvalues(colnames(distributions),from="YOJ",to="New_on_the_job")


distributions[,"New_on_the_job"] <- ifelse(distributions[,"New_on_the_job"] == 0,1,0)

NEW_IN_JOB<-distributions$New_on_the_job
prepared_data3<-cbind(prepared_data3,NEW_IN_JOB)

#korelacja zmiennych binarnych ze zmienn¹ celu 
num_unique_values_per_variable <- prepared_data3 %>%
  gather() %>%
  group_by(key) %>%
  summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

binary_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques == 2]

binary_variables_data <- prepared_data3[,binary_variables]

binary_variables_data <- data.frame(CLM_FLAG = rep(binary_variables_data$CLM_FLAG,times=(ncol(binary_variables_data) - 1)),
                                    gather(binary_variables_data[,setdiff(colnames(binary_variables_data),"CLM_FLAG")],"variable","value"),
                                    stringsAsFactors=FALSE)

binary_variables_data$value <- factor(binary_variables_data$value)

count_per_target_and_predictor <- binary_variables_data %>%
  group_by(CLM_FLAG,variable) %>%
  count(value)

count_per_target_and_predictor <- data.frame(count_per_target_and_predictor,check.names=FALSE)

count_per_predictor_level <- count_per_target_and_predictor %>%
  group_by(variable,value) %>%
  summarize(total = sum(n))

count_per_predictor_level <- data.frame(count_per_predictor_level,check.names=FALSE)

count_per_target_and_predictor <- merge(count_per_target_and_predictor,count_per_predictor_level,by=c("variable","value"))

count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$CLM_FLAG == 1,]

count_per_target_and_predictor <- data.frame(variable = count_per_target_and_predictor$variable,
                                             value = count_per_target_and_predictor$value,
                                             crash.rate = count_per_target_and_predictor$n*100/count_per_target_and_predictor$total)

ggplot(count_per_target_and_predictor,
       aes(variable,crash.rate)) +
  geom_bar(stat = "identity", aes(fill = value),position="dodge") +
  xlab("Variable") +
  ylab("CLM_FLAG_YES(%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_manual(values=c("#21618c","#f39c12"))

write.table(prepared_data3, file = "dane/prepared_data3.csv", sep = ",", row.names = FALSE, col.names = TRUE)
