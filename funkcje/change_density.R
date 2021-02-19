change_density<-function(x){
  
  for(i in seq_along(x)){
  ifelse((x[i]=="Highly Urban")|(x[i]=="Urban"), "Highly Urban/ Urban", "Highly Rural/ Rural")
 
}
  
  return(x)
}