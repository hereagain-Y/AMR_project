library(dplyr)

library(tidyverse)
#input data---------------------------------------------------------------------------------------------
question<-read.csv("C:\\Users\\Danwei Yao\\Documents\\questionaire\\question_330.csv",sep=",",header=T)
colnames(question)[1]<-"PID"
ast_April<-read.csv("C:\\Users\\Danwei Yao\\Documents\\AMR\\data\\ast_0413.csv",sep=",",header=TRUE)
colnames(ast_April)[1]<-"i_pid"
human_stool<-ast_April%>%
  filter(grepl("^C.*1S$",i_pid)&a_bacteria=="ECOLI")%>%
  select(1:5,contains("result"))


#classify the antibiotics groups 

colnames(human_stool)[6:40]=sapply(strsplit(colnames(human_stool)[6:40],"_"),function(x)x[2])
colnames(human_stool[,colSums(human_stool=="SUS"|human_stool=="RES")>0])



penicillins=c("amocla","ampicillin")#"oxacillin","penicillin")

macrolides=c("azithromycin")#"erythromycin")
quinolones=c("ciprofloxacin")#,"levofloxacin","moxifloxacin")
uncoventional=c("trisul","chlora")#"clindamycin","nitrofurantoin",
reserve=c("colistin")#,"daptomycin","linezolid")
aminoglycosides=c("gentamicin","tobramycin")#"gen500",,"st1000","tobramycin","vancomycin"
carbapenems=c("merope")#"imipenem"
#antituber=c("rifampin")
#others=c(carbapenems,antituber)
Tetracycline=c("tetracyclines")
# synerc

#****************************
# construct the check table *
#****************************
#Group the list by antimicrobial class (only inculde heterids)
peni_list=human_stool%>%
  select(i_pid,a_lab_id,amocla,ampicillin)%>%
  filter(i_pid%in%heterIDS)
macro_list=human_stool%>%
  select(1:2,all_of(macrolides))%>%
  filter(i_pid%in%heterIDS)

qunio_list=human_stool%>%
  select(1:2,all_of(quinolones))%>%
  filter(i_pid%in%heterIDS)

uncoven_list=human_stool%>%
  select(1:2,all_of(uncoventional))%>%
  filter(i_pid%in%heterIDS)

reserve_list=human_stool%>%
  select(1:2,all_of(reserve))%>%
  filter(i_pid%in%heterIDS)

ami_list=human_stool%>%
  select(1:2,all_of(aminoglycosides))%>%
  filter(i_pid%in%heterIDS)

tetra_list=human_stool%>%
  select(1:2,all_of(Tetracycline))%>%
  filter(i_pid%in%heterIDS)

car_list=human_stool%>%
  select(1:2,all_of(carbapenems))%>%
  filter(i_pid%in%heterIDS)


Class.group=list(peni_list,macro_list,qunio_list,uncoven_list,reserve_list,ami_list,tetra_list,car_list)
head(macro_list)
#*****************this is not tidy *********************
# relevel the check variables in each category 
#********************************************************
class1=peni_list%>%
         group_by(i_pid)%>%
        mutate(check=ifelse(length(unique(amocla))==1&length(unique(ampicillin))==1,"T","F"))%>%
        select(i_pid,check)%>%
        distinct(i_pid, .keep_all = TRUE)
as.data.frame(class1)

class2=macro_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(azithromycin))==1,"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)
class3=qunio_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(ciprofloxacin))==1,"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)
class4=uncoven_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(trisul))==1,"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)
class5=reserve_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(colistin))==1,"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)

class6=ami_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(gentamicin))==1&length(unique(tobramycin)==1),"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)

class7=tetra_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(tetracyclines)==1),"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)
class8=car_list%>%
  group_by(i_pid)%>%
  mutate(check=ifelse(length(unique(merope)==1),"T","F"))%>%
  select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)

peni_list[which(peni_list$i_pid=="CB007HA041S"),]

name=list(penicillins,macrolides,quinolones,uncoventional,reserve,aminoglycosides,others,Tetracycline)



diff_Ids=c(class6[which(class6$check=="F"),]$i_pid,
           class5[which(class5$check=="F"),]$i_pid,
           class4[which(class4$check=="F"),]$i_pid,

           
           
                      class3[which(class3$check=="F"),]$i_pid,
           class2[which(class2$check=="F"),]$i_pid,
           class1[which(class1$check=="F"),]$i_pid)
length(unique(diff_Ids))
table(diff_Ids)
#*******************************************************************
# calculate the frequency of different IDs appear in each category 
#*********************************************************************
myFun <- function(vector, thresh) {
  ind <- ave(rep(1, length(vector)), vector, FUN = length)
  vector[ind > thresh+1] ## added "+1" to match your terminology
}
ind <- ave(rep(4, length(diff_Ids)), diff_Ids, FUN = length)
myFun(diff_Ids,3)
myFun(diff_Ids,3)

# ave function, vector is the grouping variable
vector=c("a","b","c","d","a")

ind=ave(rep(1, length(vector)), vector, FUN = length)
vector[ind>1]

#***********************************************
## Alternative way to do use data.table 
#************************************************
library(data.table)
human_stool%>%filter(i_pid%in%group_25)
group_25=unique(as.data.table(diff_Ids)[, N := .N, by = diff_Ids][N > 0&N<3]$diff_Ids)# 1&2
group_50=unique(as.data.table(diff_Ids)[, N := .N, by = diff_Ids][N > 2]$diff_Ids)# 3/4/5times
group_75=unique(as.data.table(diff_Ids)[, N := .N, by = diff_Ids][N > 4]$diff_Ids)# only 7 sampels, 2 id s
length(unique(human_stool$i_pid))
ifelse(human_stool$i_pid%in%homoIDs,human_stool$newcheck==0,1)
human_stool$newcheck[human_stool$i_pid%in%homoIDs]<-0
human_stool$newcheck[human_stool$i_pid%in%group_25]<-1
human_stool$newcheck[human_stool$i_pid%in%group_50]<-3
human_stool$newcheck[human_stool$i_pid%in%group_miss]<-1
group_miss=unique(subset(human_stool,is.na(newcheck))$i_pid)
human_stool[which(human_stool$i_pid=="CZ021HH031S"),]
save(human_stool,file="C:\\Users\\Danwei Yao\\Desktop\\output\\Defination_two.Rdata")
load(file="C:\\Users\\Danwei Yao\\Desktop\\output\\Defination_two.Rdata")
human_stool
# need to debug ####################################################
figure.heter=function(data,name){                                  
  if(length(name)==1){
    result=data%>%
      group_by(i_pid)%>%
      mutate(check=ifelse(length(unique(name))==1,"T","F"))%>%
      select(i_pid,check)%>%
      distinct(i_pid, .keep_all = TRUE)
    return(result)
      
  }else{
    
  }
}
    
#####################################################################

#not yet find a good way 
lapply(seq(Class.group),function(x){
  
results=Class.group[[x]]%>%
   group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(all_of(name[[x]])))==1,"T","F"))%>%
    select(i_pid,check)%>%
     distinct(i_pid, .keep_all = TRUE)
   return(results)
})
#
