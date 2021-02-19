
replace_binary<-function(x){
  library(stringr)
  
 
  levels(x)[1]<-"Highly Rural/ Rural"
  levels(x)[2]<-"Highly Urban/ Urban"
  levels(x)[3]<-"Highly Rural/ Rural"
  levels(x)[4]<-"Highly Urban/ Urban"
  
  return(x)
  
 
}