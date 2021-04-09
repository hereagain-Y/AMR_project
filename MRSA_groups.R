#table1 for soci groups------------------------------------------------------
#https://cran.r-project.org/web/packages/compareGroups/vignettes/compareGroups_vignette.html
library(compareGroups)

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

mergeB=merge(ast_sum,ast_B,by.x ="i_pid",by.y="PID",all.y = T)
mergeC=inner_join(ast_sum,ast_C,c("i_pid"="PID"))


# data clean  to get  the tables, render from github ---------------------------------------

#  count number of res
count_number2=function(x){
  c=sum(x=="RES",na.rm = T)
  b=sum(x=="SUS",na.rm = T)
  result_table=cbind(RES_n=c,SUS=b)#SUS=b)
  return(result_table)
}

grouplist=list(mergeB,mergeC)
MRSA_tab=make_tab4e(grouplist,"SAURS",sum)
colnames(MRSA_tab)<-c("MSSA resistance","MSSA Sus","MRSA res","MRS Sus")
rownames(MRSA_tab)<-sapply(strsplit(rownames(MRSA_tab),"_"),function(x)x[2])
write.csv(MRSA_tab,file="~/Desktop/MRSA_table.csv")
# find a  way  to warp  together   -----------------------------------------------------------------------------
result=lapply(seq(grouplist), function(x)t(apply(grouplist[[x]][,-c(1:5)],2,function(x)count_number2(x))))             
do.call(cbind,result)

