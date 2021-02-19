stand_function<-function(x){
  x<-normalize(x, method="standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
  
  return(x)
}