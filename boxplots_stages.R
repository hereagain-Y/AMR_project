#Load libraries, data -----------------------------------------------------------

library(ggstatsplot)


# Get antibiotic length table --------------------------------------------------
n_resistance.count=function(x){
  result_table=cbind(RES_n=sum(x=="RES",na.rm = T))#SUS=b)
  return(result_table)
}

antibiolength.count=function(data,groupname){
  human_res_count<-t(t(apply(data[,-c(1:5)],MARGIN = 1,function(x)n_resistance.count(x))))
  human_res_count=as.data.frame(human_res_count)
  colnames(human_res_count)=c("RES")
  human_res_count=cbind(id=data$ï..i_pid,human_res_count,group=groupname)
  human_res_count
}
# get the a list of dasets--------------------------------------------------------
#stage data
var=c("^S.P.*","^S.W.*","^S.G.*","^S.F.*")# NO SOW HERE 
var2=c("^P.C.*","^P.X.*","^P.Y.*")

get_stage=function(dataset,var){
  stagelist<-list()
  for( i in c(1:length(var))){
    stagelist[[i]]=dataset%>%filter(grepl(var[i],ï..i_pid))
  }
  return(stagelist)
  
}

stagelist=get_stage(c_farm,var)
stagelist2=get_stage(d_farm,var2)


#set colnames ------------------------------------------------------------------------
col=c("piglet","weaner","grower","finisher")
col2=c("chicks","grower","finisher")
stagecount=list()
for (i in 1:4){
  stagecount[[i]]=antibiolength.count(stagelist2[[i]],groupname = col2[i])
  stagecount
}

# render data for plotting
countdata=do.call(rbind,stagecount)


#boxplot--------------------------------------------------------------------------------------------
countdata$group=factor(countdata$group, levels =col2)
plot=ggbetweenstats(x=group,y=RES,data=countdata,xlab="Growing Stages",ylab="Antibiogram length",pairwise.comparisons = TRUE,p.adjust.method = "fdr", ggtheme = ggplot2::theme_gray())
plot


