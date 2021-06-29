
##' @title
#' Detach all currently loaded packages
#'
#' @description
#' This code help to calculate the resistanc level from our AsT dataset
#' we can set the cut-off according to the guidelines.
#'
#' @references
#' 
#'
#' @export
#'
#' @examples \dontrun{
#' # Load some packages
#' library(dplyr)
#' library(tibble)
#' library(ggplot2)
#' 
#' # List of loaded packages -- before
#' (.packages())
#'
#' cutoff=()
#' data=human.Mic(dataset)
#' 
#'
#' 

cut_off=c()

human_june=ast_319%>%
  filter(grepl("^C.*S$",i_pid)&a_bacteria=="ECOLI")%>%
  select(!contains("result"))

human_june=data.frame(lapply(human_june, function(x){gsub("<= |< |> ","",x)}))



human.Mic<-function(humandata){
  datcount<-humandata
    datcount[,7:41]=lapply(datcount[,7:41],as.numeric)
    dat=datcount%>%
      select_if(~!is.numeric(.)||sum(.)!=0)
    example2=dat[,7:dim(dat)[2]]
    example2[,1:dim(example2)[2]]=lapply(seq(1,dim(example2)[2],by=1), function(x)example2[,x]=example2[,x]/cut_off[x])
    example2<-cbind(sample=datcount$i_pid,id=datcount$a_lab_id,example2)
    example2<-gather(example2,a_coamoxiclav_mic:a_cotrimoxazole_mic,key="colname",value = "value")
    example2$sample=substring(example2$sample,1,5)
     example2$colname=sapply(strsplit(example2$colname,"_"),function(x)x[2])
      example2$colname=factor(example2$colname,levels = c("coamoxiclav","ampicillin","Chlora.phenicol",
                                                      "cotrimoxazole","tetracyclines","gentamicin","cefoxitin","ceftazidime",
                                                      "ceftriaxone", "cefepime","ciprofloxacin","tobramycin","imipenem",
                                                      "merope","azithromycin","aztreonam","colistin"))# set column levels
  return(example2)
}


parti_mic=human.Mic(human_june)