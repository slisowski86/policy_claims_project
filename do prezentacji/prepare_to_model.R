prepare_data<-read.csv("dane/policy_before_model.csv", header=TRUE, stringsAsFactors = FALSE)
policy_original<-read.csv("dane/clear_data.csv", sep=",", header=TRUE)

summary(policy_original)
#usuwamy kolumny 

library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(gridExtra)
library(caret)
library(pROC)

names<-c("AGE.GENDER", "AGE_YEARS")

to_delete<-col_to_change(prepare_data, names)

prepare_data<-prepare_data[-to_delete]
summary(prepare_data)

#kolumnê density zmieniamy na factor z dwoma poziomami



summary(prepare_data$DENSITY)

prepare_data$DENSITY<-plyr::mapvalues((prepare_data$DENSITY),
from=c("Highly Urban", "Urban", "Highly Rural", "Rural"),
to=c("Highly Urban/ Urban", "Highly Urban/ Urban", "Highly Rural/ Rural", "Highly Rural/ Rural" ))

#kolumny z wartoœciami binarnymi oraz DENSITY zamieniamy na TRUE FALSE
colnames(prepare_data) <- plyr::mapvalues(colnames(prepare_data),
 from=c("CAR_USE","RED_CAR","REVOKED","CLM_FLAG","GENDER","MARRIED", "PARENT1", "DENSITY" ),
 to=c("COMMERCIAL_VEHICLE","RED_CAR","REVOKED","CLM_FLAG","SEX_MALE", "MARRIED", "SINGLE_PARENT", "URBAN_NOT_RURAL" ))

binary_variable_translations <- data.frame(Variable = c("COMMERCIAL_VEHICLE","RED_CAR","REVOKED","CLM_FLAG","SEX_MALE","MARRIED", "SINGLE_PARENT", "URBAN_NOT_RURAL"),
True.value = c("Commercial","yes","Yes","Yes","M","Yes","Yes","Highly Urban/ Urban"),
 stringsAsFactors=FALSE)



for(i in 1:length(binary_variable_translations$Variable))
{
 var <- binary_variable_translations$Variable[i]
 true_value <- binary_variable_translations$True.value[i]
 prepare_data[,var] <- ifelse(prepare_data[,var] == true_value,1,0)
}

#Tworzymy rozk³ad wartoœci binarnych
prepare_data[,c("COMMERCIAL_VEHICLE","RED_CAR","REVOKED","CLM_FLAG","MARRIED","SEX_MALE", "SINGLE_PARENT", "URBAN_NOT_RURAL")] %>%
 gather("variable","value") %>%
group_by(variable) %>%
 count(value) %>%
mutate(value = factor(value)) %>%
mutate(percent = n*100/10296) %>%
ggplot(.,
   aes(variable,percent)) +
 geom_bar(stat = "identity", aes(fill = value) ) +
 xlab("Variable") +
 ylab("Percent of records") +
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   scale_fill_manual(values = rev(c("#E69F00","#999999")))

#Tworzymy rozk³ady dla zmiennych dyskretnych i faktorów

policy_original[,c("CLM_FREQ","HOMEKIDS","KIDSDRIV")] %>%
   gather("variable","value") %>%
   group_by(variable) %>%
   count(value) %>%
   mutate(value = factor(value,levels=5:0)) %>%
   mutate(percent = n*100/10296) %>%
   ggplot(.,
          aes(variable,percent)) +
   geom_bar(stat = "identity", aes(fill = value)) +
   xlab("Variable") +
   ylab("Percent of records") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
   scale_fill_manual(values = rev(c("#1E90FF","#F0E68C", "#FFA500", "#20B2AA", "#CD5C5C","#4B0082")))


# wartoœci puste dla Job zamieniamy na Other
prepare_data$JOBCLASS[prepare_data$JOBCLASS == ""] <- "Other"

# Tworzymy rozk³ady zmiennych dla faktorów

for(var in c("CAR_TYPE","MAX_EDUC","JOBCLASS"))
{
   frequency_per_level <- data.frame(table(prepare_data[,var]))
   colnames(frequency_per_level) <- c("value","n")
   
   print(ggplot(frequency_per_level,
                aes(value,n*100/10296)) +
            geom_bar(stat = "identity",col="black",fill="darkgrey") +
            xlab("") +
            ylab("Percent of records") +
            ggtitle(var) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            scale_fill_manual(values= "#BDB76B"))
}

#wartoœæ w kolumnie MVR_PTS skalujemy do 10

prepare_data$MVR_PTS <- ifelse(prepare_data$MVR_PTS > 10,10,prepare_data$MVR_PTS)
summary(prepare_data$MVR_PTS)

frequency_per_level <- data.frame(table(prepare_data[,"MVR_PTS"]))
colnames(frequency_per_level) <- c("value","n")
#frequency_per_level$value <- factor(frequency_per_level$value,levels=as.numeric(frequency_per_level$value)[order(as.numeric(frequency_per_level$value))])
frequency_per_level$value <- factor(frequency_per_level$value,levels=c(0:10,11,13))

ggplot(frequency_per_level,
       aes(value,n*100/10296)) +
   geom_bar(stat = "identity",col="black",fill="darkgrey") +
   xlab("") +
   ylab("Percent of records") +
   ggtitle("MVR_PTS") +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))


policy_car_age<-read.csv("dane/policy_integer_binary.csv", header=TRUE, stringsAsFactors = FALSE)
car_age<-c(as.numeric(difftime(policy_car_age$PLCYDATE, policy_car_age$INITDATE, units="weeks")))
car_age<-car_age/52
car_age<-round(car_age, digits=0)
AGE_CAR<-car_age
prepare_data<-cbind(prepare_data, AGE_CAR)

write.table(prepare_data, file = "dane/prepared_data1.csv", sep = ",", row.names = FALSE, col.names = TRUE)
