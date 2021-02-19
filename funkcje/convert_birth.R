convert_birth<-function(x){
  
  library(stringi)
   stri_sub(x,7,6)<-19
   
   return(x)
}