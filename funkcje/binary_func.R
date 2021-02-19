binary_func<-function(x){
  for(i in seq(x)){
    if(x[i]>0.5){
      x[i]==1
    }else{
      x[i]==0
    }
    
  }
  
  return(x)
}