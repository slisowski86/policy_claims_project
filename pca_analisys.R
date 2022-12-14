#u?ywamy bazy ze znormalizowanumi zeminnymi do analizy PCA

policy_clear<-read.csv("policy_after_clear.csv", sep=",", header=TRUE)
summary(policy_clear)

#przesuwamy kolumn? ze zmienn? celu na ostatni? pozycj? w ramce danych
policy_clear<-policy_clear[,c(13, 1:26)]
policy_clear<-policy_clear[-14]

#za pomoc? funkcji prcomp liczymy sk?adowe g??wne

policy_pca<-prcomp(policy_clear[,2:26], cente=TRUE, scale=TRUE)

summary(policy_pca) 

library(factoextra)

#stosujemy wykres osuwiskowy aby dobra? odpowiedni? liczb? sk?adowych g??wnych

#fviz_eig(policy_pca)

#wed?ug wykresu do analizy bierzemy 6 sk?adowych g??wnych

fviz_pca_var(policy_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_contrib(policy_pca, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(policy_pca, "var", axes = 2)
fviz_contrib(policy_pca, "var", axes = 3)
# Contribution to the second dimension
fviz_contrib(policy_pca, "var", axes = 4)
fviz_contrib(policy_pca, "var", axes = 5)
# Contribution to the second dimension




library(corrplot)

m<-cor(policy_clear)
corrplot(m, method ="number")

