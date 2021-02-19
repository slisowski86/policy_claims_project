na_replace<-function(x){
  x[c(which(is.na(x)))]<-mean(x, na.rm=TRUE)  
  
  return(x)
}