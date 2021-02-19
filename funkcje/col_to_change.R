col_to_change<-function(x,y){
 
  z<-c()
  for(i in seq_along(colnames(x))){
    for(j in seq_along(y)){
      if(y[j]==colnames(x[i])){
       n<- match(y[j], names(x))
        z<-c(z,n)
      }
    }
  }
  
  return(z)
}
