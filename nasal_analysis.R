#table1 for soci groups------------------------------------------------------
#https://cran.r-project.org/web/packages/compareGroups/vignettes/compareGroups_vignette.html
library(compareGroups)
#socigroup$group<-factor(socigroup$group,levels = c("A","B","C"))
res<-compareGroups(group~.,data=socigroup)#method = c(wtn=2,pl4=2))
export_table=createTable(res,hide.no = "0")
export2word(export_table,file="~/Desktop/groups_table1.docx",strip = T)
restab<-createTable(res,digits = c(pl4=0,hormo=1),type=1,hide = c(sex = "1"), hide.no = "no", show.n = TRUE)


# for life style summary table --------------------------------------------
life_t<-compareGroups(group~.,data=lifestyle)
life_table=createTable(life_t,hide.no = "0")
export2word(life_table,file="~/Desktop/lifestyle_table1.docx",strip = T)
# for knowledge summary table -------------------------------------------
dim(knowledge[complete.cases(knowledge),])
dim(knowledge)
know_t<-compareGroups(group~.,data=knowledge)
help("compareGroups")
know_table=createTable(know_t,hide.no = "0")
update(know_table,hide=c("ABX_kill_virus_BL"="NA"))
help("createTable")
export2word(know_table,file="~/Desktop/knowledge_table1.docx",strip = T)
# for use of antibotics-------------------------------------------------------
use_t<-compareGroups(group~.,data=usage)
use_table=createTable(use_t,hide.no =c("NA","0"))
export2word(use_table,file="~/Desktop/use_table1.docx",strip = T)
# for health  status----------------------------------------------------------
health_t<-compareGroups(group~.,data=health)
health_table=createTable(health_t,hide.no = "0")
export2word(health_table,file="~/Desktop/health_table1.docx",strip = T)
# warp all the data  togtehr -----------------------------------------------
word_summary<-function(data){
  table_t=compareGroups(group~.,data=data)
  render_table=createTable(table_t,hide.no = "0")
  render_table
}
#create a list for all the data
list1<-list(health,lifestyle)
# make compare table for every data in the list 
list2=lapply(list1, word_summary)
# name it 
names(list2)<-c("health","lifestyle")
# export them to word 
sapply(seq(list2), function(x)export2word(list2[[x]],file=paste0("~/Desktop/",names(list2)[x],".docx",sep=''),strip=T))

#  comparison  of resistance and sus of two groups.--------------------------------------------------------------------------------------
ast_B=groupB%>%
  select(PID,group)
ast_C=groupC%>%
  select(PID,group)
save(total,file="~/Desktop/2996_ast.Rdata")
# merge  two tables together 
ast_sum<-total%>%
  filter(grepl("^C.*1N$",i_pid)&a_bacteria=="SAURS")%>%
  select(1:5,contains("result"))
ast_sum$i_pid<-substring(ast_sum$i_pid,1,5)
dim(ast_group)
dim(ast_sum)
mergeB=merge(ast_sum,ast_B,by.x ="i_pid",by.y="PID",all.y = T)
mergeC=inner_join(ast_sum,ast_C,c("i_pid"="PID"))
dim(mergeB)
dim(mergeC)

# data clean  to get  the tables, render from github ---------------------------------------
n_resistance.count(mergedata2)
#  count number of res
count_number2=function(x){
  c=sum(x=="RES",na.rm = T)
  b=sum(x=="SUS",na.rm = T)
  result_table=cbind(RES_n=c,SUS=b)#SUS=b)
  return(result_table)
}

sum=c(148,45)
make_tab4e<-function(data,bacteria,sum){
  datalist<-list()
  for (i in c(1:length(data))){
    table=subset(data[[i]],a_bacteria==bacteria)
    tmp=t(apply(data[[i]][,-c(1:5)],2,function(x)count_number2(x)))
    datalist[[i]]<-round(tmp/sum[i],3)*100
    
  }
  datalist=do.call(cbind,datalist)
  colname=c("B_R","BS","CR","CS")#,"Local","Unknown")
  colnames(datalist)=colname
  datalist
}
grouplist=list(mergeB,mergeC)
MRSA_tab=make_tab4e(grouplist,"SAURS",sum)
colnames(MRSA_tab)<-c("MSSA resistance","MSSA Sus","MRSA res","MRS Sus")
rownames(MRSA_tab)<-sapply(strsplit(rownames(MRSA_tab),"_"),function(x)x[2])
write.csv(MRSA_tab,file="~/Desktop/MRSA_table.csv")
# find a  way  to warp  together   -----------------------------------------------------------------------------
result=lapply(seq(grouplist), function(x)t(apply(grouplist[[x]][,-c(1:5)],2,function(x)count_number2(x))))             
do.call(cbind,result)

