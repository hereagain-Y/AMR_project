#' @title Count antibiogram length
#' @name antibiolength.count()
#'
#' @description
#' # to get the antibiogram length from sampel data results in a dataframe
#' antibiolength.count(
#'   
#' )
#' }
#' @example




n_resistance.count=function(x){
  result_table=cbind(RES_n=sum(x=="RES",na.rm = T))#SUS=b)
  return(result_table)
}

antibiolength.count=function(data){
  human_res_count<-t(t(apply(data[,-c(1:5)],MARGIN = 1,function(x)n_resistance.count(x))))
  human_res_count=as.data.frame(human_res_count)
  colnames(human_res_count)=c("RES")
  human_res_count=cbind(id=data$i_pid,lab_id=data$a_lab_id,human_res_count)
  human_res_count
}
