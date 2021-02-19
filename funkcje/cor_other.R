prepare_data7<-read.csv("dane/data_transformed2.csv", header=TRUE, stringsAsFactors = FALSE)

#korelacja Commercial Use vs CLM_AMT

boxplot(log10(CLM_AMT) ~ factor(CAR_TYPE,levels=c("Sedan","Sports Car","SUV","Pickup","Van","Panel Truck")),
        xlab="Car type",
        ylab="log10(CLM_AMT)",
        data=prepare_data7[prepare_data7$CLM_FLAG == 1,])

par(mfrow=c(1,2))

boxplot(log10(CLM_AMT) ~ factor(Commercial_vehicle),
        xlab="Commercial vehicle",
        ylab="log10(CLM_AMT)",
        data=prepare_data7[prepare_data7$CLM_FLAG == 1,])

boxplot(log10(CLM_AMT) ~ factor(COMMERCIAL_VEHICLE),
        xlab="Commercial vehicle",
        ylab="log10(CLM_AMT)",
        data=prepare_data7[prepare_data7$CLM_FLAG == 1 & prepare_data7$CLM_AMT >= 1000,],
        main="CLM_AMT >= $1,000 only")
        
#Korelacja zmiennych binarnyc z CLM_AMT
num_unique_values_per_variable <- prepare_data7 %>%
  gather() %>%
  group_by(key) %>%
  summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

## ----crash-rate-binary-vars, echo=FALSE, eval=TRUE-----------------------
binary_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques == 2]
correlations_binary_vs_target_amt <- c()

for(var in binary_variables)
{
  correlations_binary_vs_target_amt <- c(correlations_binary_vs_target_amt,cor(prepare_data7[prepare_data7$CLM_FLAG == 1,var],log10(prepare_data7[prepare_data7$CLM_FLAG == 1,"CLM_AMT"]),use="pairwise.complete.obs"))
}

correlations_binary_vs_target_amt <- data.frame(Variable = binary_variables,
                                                Correlation = round(correlations_binary_vs_target_amt,digits=2),
                                                stringsAsFactors=FALSE)


correlations_binary_vs_target_amt[order(correlations_binary_vs_target_amt$Correlation),]

#dodajemy zmienn¹ abstrakcyjn¹ „Car_type_for_claim_amount”

prepare_data7 <- data.frame(prepare_data7,
                        Car_type_for_claim_amount = ifelse(prepare_data7$CAR_TYPE != "Van" & prepare_data7$CAR_TYPE != "Panel Truck",0,1),
                        check.names=FALSE,stringsAsFactors=FALSE)

prepare_data7$Car_type_for_claim_amount <- ifelse(prepare_data7$CAR_TYPE == "Panel Truck",2,prepare_data7$Car_type_for_claim_amount)

#dodajemy zmienn¹ Marriage Home Kids

marriage_home_and_kids_score_policy <- rep(8,times=nrow(prepare_data7))

marriage_home_and_kids_score_policy <- ifelse(prepare_data7$MARRIED == 0,marriage_home_and_kids_score_policy - 1,marriage_home_and_kids_score_policy)
marriage_home_and_kids_score_policy <- ifelse(prepare_data7$Homeowner == 0,marriage_home_and_kids_score_policy - 2,marriage_home_and_kids_score_policy)
marriage_home_and_kids_score_policy <- ifelse(prepare_data7$SINGLE_PARENT == 1,marriage_home_and_kids_score_policy - 3,marriage_home_and_kids_score_policy)
marriage_home_and_kids_score_policy <- ifelse(prepare_data7$SINGLE_PARENT == 1 & prepare_data7$Homeowner == 0,marriage_home_and_kids_score_policy - 2,marriage_home_and_kids_score_policy)


prepare_data7 <- data.frame(prepare_data7,
                        marriage_home_and_kids_score = marriage_home_and_kids_score_policy,
                        check.names=FALSE,stringsAsFactors=FALSE)   

write.table(prepare_data7, file = "dane/data_transformed_all.csv", sep = ",", row.names = FALSE, col.names = TRUE)
