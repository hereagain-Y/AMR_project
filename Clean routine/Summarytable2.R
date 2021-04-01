# codes for obtaing table 2----------------------------------------------------
library(dplyr)
library(xlsx)
ast_319<-read.csv( "C:\\Users\\Danwei Yao\\Documents\\AMR\\data\\astresult0319.csv",sep=",",header=T)
ast_319=ast_319%>%
dplyr::select(1:5,contains("result"))

# retain a new dataset----------------------------------------------------------

##########for bacterial without guidelines--------------------------------------
n_resistance.count=function(x){
  result_table=cbind(RES=sum(x=="RES",na.rm = T))#SUS=b)
  return(result_table)
}
n_resistance.count()

# calculate the total isolates for each sample as dinominator-------------------
num<-vector()
find_number=function(x){
  bac_data=ast_319%>%
    filter(a_bacteria==x)
  for (i in 1:15){
    order=c("^92","^93","^13","^14","^17","^18","^19","^23","^24","^25","^26","^31","^33","^35","^36")
    data=bac_data%>%filter(grepl(order[i],a_lab_id))
    num[i]=nrow(data)
  }
  return(num)
}
number_1=find_number("ECOLI")
number_2=find_number("SALSP")
number_3=find_number("SAURS")
number_4=find_number("KPNEU")

number_5=find_number("EFAEL")
number_6=find_number("EFAEM")

#make table 2-------------------------------------------------------------------
datalist<-list()
table<-data.frame()
#for sm and esl
make_table2<-function(x,y,z){
  datalist<-list()
  table=subset(x,a_bacteria==y)
  for(i in 1:15){
    order=c("^92","^93","^13","^14","^17","^18","^19","^23","^24","^25","^26","^31","^33","^35","^36")
    data=table%>%filter(grepl(order[i],a_lab_id))
    tmp=t(t(apply(data[,-c(1:5)],2,function(x)n_resistance.count(x))))
    datalist[[i]]<-round(tmp/z[i],2)*100# use the amount/ sum 
    
  }
  datalist=do.call(cbind,datalist)
  samplenames<-c("Human stool","Human nasal","Swine stool","Swine sludge","Poultry swab","Poultry fecal","Poultry sludge","Swine iffluent water","Swine effluet","River","Beach","Animal local","Animal imported","Animal unknown","Vege unknown")
  
  colnames(datalist)=samplenames
  datalist[,which(is.na)]<-0
  datalist
}

ecoli1=make_table2(ast_319,"ECOLI",number_1)
salsp2=make_table2(ast_319,"SALSP",number_2)
saurs3=make_table2(ast_319,"SAURS",number_3)
kpneu4=make_table2(ast_319,"KPNEU",number_4)
efael5=make_table2(ast_319,"EFAEL",number_5)
efaem6=make_table2(ast_319,"EFAEM",number_6)



bacteria_list<-list(ecoli1,salsp2,saurs3,kpneu4,efael5,efaem6)
bacteria_name=c("ECOLI","SALSP","SAURS","KPNEU","EFAEL","EFAEM")
append_bacteria=function(x,y){
  write.xlsx(x[[1]],file="C:\\Users\\Danwei Yao\\Documents\\AMR\\Bacteria_res.xlsx",sheetName = "Ecoli",row.names = T,showNA = F)
  for( i in 2:6){
    write.xlsx(x[[i]],file="C:\\Users\\Danwei Yao\\Documents\\AMR\\Bacteria_res.xlsx",sheetName=y[i] ,append=T,row.names=T,showNA = F)
  }
}


append_bacteria(bacteria_list,bacteria_name)



#try for example 
sal=anti_result%>%
  filter(a_bacteria=="SALSP")

data=sal%>%
  filter(grepl("^93",a_lab_id))

temp=t(apply(data[,-c(1:6)],2,function(x)count_number2(x)))
round(temp/5,2)*100

names=rep(c("SUS","INT","RES"),15)
samplenames<-c("Human stool","Human nasal","Swine stool","Swine sludge","Poultry swab","Poultry fecal","Poultry sludge","Swine iffluent water","Swine effluet","River","Beach","Animal local","Animal imported","Animal unknown","Vege unknown")
lala=rep(samplenames,each=3)
paste0(names," ",lala,"%")



