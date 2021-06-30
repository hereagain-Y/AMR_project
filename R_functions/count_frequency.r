# @name count_frequency()
#' description:
#' caculate the duplicated frequency of every element in a vector, and select the elements that meet our frequency.
#' In this scenario, we are going to classify the Multidrug resistance Ids accoring to their frequencies in every Antibiotic class.
# also can do it via data.table




count_frquency<-function(variable,thresh,endpoint){
    ind<-ave(rep(1,length(variable)),variable,FUN=length)
    variable[ind>thresh&ind<endpoint]
  }
  
  level1=unique(count_frquency(diff_id,3,5))
  
  
  #same way as above
  library(data.table)
level1=unique(as.data.table(diff_id)[,N:=.N,by=diff_id][N==1]$diff_id)
