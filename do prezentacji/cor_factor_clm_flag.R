prepare_data4<-read.csv("dane/prepared_data3.csv", header = TRUE, stringsAsFactors = FALSE)
to_change<-read.csv("dane/original_values.csv", header = TRUE, stringsAsFactors = FALSE)
copy_pd4<-prepare_data4

names<-c("TRAVTIME", "BLUEBOOK", "RETAINED", "OLDCLAIM", "CLM_AMT", "YOJ", "INCOME", "HOME_VAL", "SAMEHOME", "CAR_AGE")

indexes<-col_to_change(prepare_data4, names)

indexes_tc<-col_to_change(to_change, names)

prepare_data4[indexes]<-to_change[indexes_tc]

num_unique_values_per_variable <- prepare_data4 %>%
  gather() %>%
  group_by(key) %>%
  summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

factor_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques > 2 & num_unique_values_per_variable$Num.uniques < 10]

factor_variables_data <- prepare_data4[,factor_variables]

factor_variables_data <- data.frame(CLM_FLAG = rep(prepare_data4$CLM_FLAG,times=ncol(factor_variables_data)),
                                    gather(factor_variables_data,"variable","value"),
                                    stringsAsFactors=FALSE,check.names=FALSE)

count_per_target_and_predictor <- factor_variables_data %>%
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

for(var in unique(count_per_target_and_predictor$variable))
{
  print(ggplot(count_per_target_and_predictor[count_per_target_and_predictor$variable == var,],
               aes(value,crash.rate)) +
          geom_bar(stat = "identity") +
          xlab("") +
          ylab("CLM_FLAG_YES(%)") +
          ggtitle(var) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          scale_fill_manual(values="#A569BD"))
}

write.table(prepare_data4, file = "dane/prepared_data5.csv", sep = ",", row.names = FALSE, col.names = TRUE)
