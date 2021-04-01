cal_isolandsample<-function(x){
  result=x%>%group_by(a_bacteria)%>%
    summarise(n_isolate=n(),number_of_sample=n_distinct(ï..i_pid))
  return(result)
}
file.choose()
ast_319<-read.csv( "C:\\Users\\Danwei Yao\\Documents\\AMR\\result_0323.csv",sep=",",header=T)

colnames(ast_319)[1]

result_list<-list()

make_summarytable=function(x){
  for(i in 1:15){
    order=c("^92","^93","^13","^14","^17","^18","^19","^23","^24","^25","^26","^31","^33","^35","^36")
    data=x%>%filter(grepl(order[i],a_lab_id))
    result_list[[i]]=cal_isolandsample(data)

    
  }
  return(result_list)
  }
list1=make_summarytable(ast_319)
list1[[1]]


library(xlsx)

write.xlsx(list1[[1]],file="C:\\Users\\Danwei Yao\\Documents\\AMR\\Summaryast.xlsx",sheetName = "Human stool",row.names = T)
sheet_name<-c("Human stool","Human nasal","Swine stool","Swine sludge","Poultry swab","Poultry fecal","Poultry sludge","Swine iffluent water","Swine effluet","River","Beach","Animal local","Animal imported","Animal unknown","Vege unknown")
append_excel=function(x,y){
  write.xlsx(list1[[1]],file="C:\\Users\\Danwei Yao\\Documents\\AMR\\Summaryast.xlsx",sheetName = "Human stool",row.names = T)
  for( i in 2:15){
    write.xlsx(x[[i]],file="C:\\Users\\Danwei Yao\\Documents\\AMR\\Summaryast.xlsx",sheetName=y[i] ,append=T,row.names=T)
  }
}
append_excel(list1,sheet_name)



