replace_levels<-function(x){
 
  n<-nlevels(x)
  step<-1/n
  stepr<-round(step, digits=3)
  rest<-stepr%%0.01
  stepok<-stepr-rest
  lev<-seq(stepok,1,by=stepok)
  for(i in seq_along(lev)){
    
    levels(x)[i]<-(lev[i])
  }
  
 
  
  return(x)
}

