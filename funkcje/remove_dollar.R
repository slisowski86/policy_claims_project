remove_dollar<-function(x){
  x<-as.numeric(gsub("[\\$]", "", x))
return(x)
}
