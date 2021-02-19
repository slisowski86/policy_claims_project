

change_dates<-function(x){
  french_months<-c("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout", "septembre",
                   "octobre", "novembre", "decembre")
  good_months<-c("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-")
  
  
  
  for(i in seq_along(french_months)){
    s<-gsub("[0-9]","", x)
    if(s==french_months[i]){
      x<-gsub(s,good_months[i],x)
    }
    
  }
  
  return(x)
  }
  
  
