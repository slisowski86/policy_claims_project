library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(caret)
library(pROC)

var_cor_data<-read.csv("dane/data_transformed_all.csv", header = TRUE, stringsAsFactors = FALSE)
num_unique_values_per_variable <- var_cor_data %>%
  gather() %>%
  group_by(key) %>%
  summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable$Variable <- as.vector(num_unique_values_per_variable$Variable)

binary_variables <- num_unique_values_per_variable$Variable[num_unique_values_per_variable$Num.uniques == 2]

binary_variables <- setdiff(binary_variables,c("CLM_FLAG","RED_CAR","SEX_MALE"))

## ----correlations-within-binary, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
binary_variables_data <- var_cor_data[,binary_variables]

correlation_variables <- abs(cor(binary_variables_data,use="pairwise.complete.obs"))

# tworzymy macierz korelacji

for(i in 1:(nrow(correlation_variables) - 1))
{
  correlation_variables[i,seq(from=i,to=ncol(correlation_variables),by=1)] <- NA
}

correlation_variables[ncol(binary_variables_data),ncol(binary_variables_data)] <- NA

correlation_variables <- gather(data.frame(correlation_variables),"y","correlation")

correlation_variables <- data.frame(x = rep(unique(correlation_variables$y),times=length(unique(correlation_variables$y))),correlation_variables)

correlation_variables$x <- factor(correlation_variables$x,levels=colnames(binary_variables_data))
correlation_variables$y <- factor(correlation_variables$y,levels=colnames(binary_variables_data))

correlation_variables %>%
  ggplot(.,
         aes(x = x,y = y)) +
  geom_tile(aes(fill = correlation)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
  geom_text(aes(label = round(correlation, 2)),size=2) +
  ggtitle("Pokazane jedynie wartoœci bezwglêdne")

#Korelacja miêdzy zmiennymi numerycznymi

pairs(var_cor_data[,c("INCOME","BLUEBOOK","TRAVTIME")],lower.panel=NULL,cex=0.5)

print("Korelacja INCOME i BLUEBOOK:")
round(cor(var_cor_data$INCOME,var_cor_data$BLUEBOOK,use="pairwise.complete.obs"),digits=2)
print("Korelacja INCOME i TRAVTIME:")
round(cor(var_cor_data$INCOME,var_cor_data$TRAVTIME,use="pairwise.complete.obs"),digits=2)
print("Korelacja TRAVTIME i BLUEBOOK:")
round(cor(var_cor_data$BLUEBOOK,var_cor_data$TRAVTIME,use="pairwise.complete.obs"),digits=2)

#Korelacje miêdzy zmiennymi numerycznymi a binarnymi

correlations_income_vs_binary_var <- c()


for(var in binary_variables)
{
  correlations_income_vs_binary_var <- c(correlations_income_vs_binary_var,cor(var_cor_data[,var],var_cor_data$INCOME,use="pairwise.complete.obs"))
}

correlations_income_vs_binary_var <- data.frame(Variable = binary_variables,
                                                Correlation = round(correlations_income_vs_binary_var,digits=2),
                                                stringsAsFactors=FALSE)
correlations_income_vs_binary_var[order(correlations_income_vs_binary_var$Correlation),]

correlations_travtime_vs_binary_var <- c()

for(var in binary_variables)
{
  correlations_travtime_vs_binary_var <- c(correlations_travtime_vs_binary_var,cor(var_cor_data[,var],var_cor_data$TRAVTIME,use="pairwise.complete.obs"))
}

correlations_travtime_vs_binary_var <- data.frame(Variable = binary_variables,
                                                  Correlation = round(correlations_travtime_vs_binary_var,digits=2),
                                                  stringsAsFactors=FALSE)

correlations_travtime_vs_binary_var[order(correlations_travtime_vs_binary_var$Correlation),]


correlations_clmamt_vs_binary_var <- c()
for(var in binary_variables)
{
  correlations_clmamt_vs_binary_var <- c(correlations_clmamt_vs_binary_var,cor(var_cor_data[,var],var_cor_data$CLM_AMT,use="pairwise.complete.obs"))
}

correlations_clmamt_vs_binary_var <- data.frame(Variable = binary_variables,
                                                  Correlation = round(correlations_clmamt_vs_binary_var,digits=2),
                                                  stringsAsFactors=FALSE)

correlations_clmamt_vs_binary_var[order(correlations_clmamt_vs_binary_var$Correlation),]
