convert_dates<-function(x){
  Encoding(x)
    s<-stri_trans_general(x,"latin-ascii")
   x<-s
  }