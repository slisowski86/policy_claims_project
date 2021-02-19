policy_plot<-read.csv("dane/policy_factor.csv", sep=",", header =TRUE, stringsAsFactors=FALSE)
summary(policy_plot)

policy_join<-read.csv("policy_before_na.csv",  sep=",", header =TRUE, stringsAsFactors=FALSE)

#dodajemy kolumnê CAR_AGE
CAR_AGE<-policy_join$PLCY_YEARS
policy_plot<-cbind(policy_plot,CAR_AGE)

summary(policy_plot$CAR_AGE)
table(policy_plot$CAR_AGE)
cat_carage<-cut(policy_plot$CAR_AGE, breaks=c(0,7,14,21,28), labels=c("0-7", "7-14", "14-21", "21-28"), right=FALSE)
policy_plot$CAR_AGE<-cat_carage


library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(caret)
library(pROC)
num_unique<- policy_plot %>%
  
  
  gather() %>%
  group_by(key) %>%
  summarize(uniques = n_distinct(value))

num_unique<-data.frame(num_unique)

num_unique<- data.frame(Variable = num_unique[,1],Num.uniques = num_unique[,2],stringsAsFactors=FALSE)
summary



factor_variables <- num_unique$Variable[num_unique$Num.uniques > 2 & num_unique$Num.uniques < 15]
factor_variables

factor_variables_data<-policy_plot[,factor_variables]
factor_variables_data
factor_variables_data <- data.frame(CLM_FLAG = rep(policy_plot$CLM_FLAG,times=ncol(factor_variables_data)),
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
count_per_target_and_predictor <- count_per_target_and_predictor[count_per_target_and_predictor$CLM_FLAG == "Yes",]
count_per_target_and_predictor <- data.frame(variable = count_per_target_and_predictor$variable,
                                             value = count_per_target_and_predictor$value,
                                             crash.rate = count_per_target_and_predictor$n*100/count_per_target_and_predictor$total)
for(var in unique(count_per_target_and_predictor$variable))
{
  print(ggplot(count_per_target_and_predictor[count_per_target_and_predictor$variable == var,],
               aes(value,crash.rate)) +
          geom_bar(stat = "identity") +
          xlab("") +
          ylab("CLAIM_YES (%)") +
          ggtitle(var) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)))
  
}

write.table(policy_plot, file = "dane/policy_before_model.csv", sep = ",", row.names = FALSE, col.names = TRUE)
