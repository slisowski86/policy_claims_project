norm_func<-function(x){
  normalize(x, method="range",range=c(0,1), margin = 1L, on.constant = "quiet")
}