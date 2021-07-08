#' @ generate the sequence data form for future selection 
#' we add age, gender, abx_use frequency, MDR-pattern, MRSA,whether resistant to reserve abx, husehold information and ast result in to dataset
#require packadges
library(readxl)
library(dplyr)


# required data
ast=read_excel("C:\\Users\\Danwei Yao\\Desktop\\AST_0702.xlsx"
)
qustion=read_excel("C:\\Users\\Danwei Yao\\Desktop\\Questionnaire_20210618.xlsx")
                  
sequence_ast=ast
people=substring(unique(sequence_ast$i_pid),1,5)
ast_ques=qustion%>%
  filter(PID%in%people)


# gender random household ID for every household
df1=ast_ques%>%
  filter(house_part1_BL!="NA")%>%
  select(PID,house_part1_BL:house_part5_BL)
# remove () in every column
df1[,2:6]<-sapply(df1[,2:6],function(x)gsub("\\(.*\\)","",x))

# substreing the Pid
testdata=as.data.frame(apply(df1,2,function(x)substring(x,2,5)))
testdata$max_id=as.vector(apply(testdata,1,max))


group_id <- testdata %>% group_indices(max_id) 

testdata=testdata %>%
  mutate(household=paste("house_hold",group_id,""))
testdata$max_id=gsub(" ", "", testdata$max_id, fixed = TRUE)
                             
# add a house hold column 
ast_ques$new_house=ifelse(ast_ques$house_part1_BL=='NA',"no",testdata$household)

new_df=data.frame(PID=ast_ques$PID,Age=ast_ques$age_BL,Gender=ast_ques$gender_BL,Household=ast_ques$new_house,Use_abx=ast_ques$course_year_BL,Ever_use=ast_ques$ever_taken_ABX_BL)




#---------------------------------------------------------
cat("--------------for MDR and MRSA-------------")

MSSA=ast%>%
  filter(grepl("^C.*1N$",i_pid)&a_oxacillin_result=="SUS")%>%
  select(i_pid, a_lab_id)
MRSA=ast%>%
  filter(grepl("^C.*1N$",i_pid)&a_oxacillin_result=="RES")%>%
  select(i_pid, a_lab_id)
mssa_id=MSSA$a_lab_id
mrsa_id=MRSA$a_lab_id

human_ast$MRSA=ifelse(human_ast$a_lab_id%in%mssa_id,"MSSA",
                      ifelse(human_ast$a_lab_id%in%mrsa_id,"MRSA",
                             "No"))

#-------------------------------------------
human_ast=ast%>%
  filter(grepl("^C.*",i_pid))%>%
  select(1:5,contains("result"))
#------------------------------------------

#count MDR frquency
#
ecoli=human_ast%>%
  filter(a_bacteria=="ECOLI")%>%
  select_if(~!is.numeric(.)||sum(.)!=0)
glimpse(ecoli)
colnames(human_ast)[6:40]=sapply(strsplit(colnames(human_ast)[6:40],"_"),function(x)x[2])


# Abx acategory




rifamycins=c("rifampin")
Tetracycline=c("tetracyclines")
sulfonamide=c("trisul")
phenicol=c("chlora")
lincosamides=c("clindamycin")
nitro=c("nitrofurantoin")

category_list=list(lactams,macrolides,aminoglycosides,quinolones,carbapenems,
                   Tetracycline,rifamycins,sulfonamide,phenicol,lincosamides,nitro,reserve)



mdr_id=sapply(1:8, function(x){
  data=human_ast[,c("i_pid","a_lab_id",category_list[[x]])]
  id=unique(data[apply(data, 1, function(x)any(x=="RES")),]$a_lab_id)
  id
})


mdr_id=unlist(mdr_id)
mdr_id=mdr_id[!is.na(mdr_id)]
count_frquency<-function(variable,thresh,endpoint){
  ind<-ave(rep(1,length(variable)),variable,FUN=length)
  variable[ind>thresh&ind<endpoint]
}



# data.table(mdr_id)[, .N, mdr_id] count the times of duplicate
No_mdr=unique(count_frquency(mdr_id,0,4))#<=3
Mdr= unique(count_frquency(mdr_id,3,8))#<=3
human_ast$resistance=ifelse(human_ast$a_lab_id%in%No_mdr,"No MDR",
                  ifelse(human_ast$a_lab_id%in%Mdr,"MDR","All SUS"))
#--------------
#reserve

reserve_list=human_ast%>%
  select(1:2,all_of(reserve))
res_id=reserve_list[apply(reserve_list, 1, function(x)sum(x=="RES",na.rm=T)>=1),]$a_lab_id
reserve_list[apply(reserve_list, 1, function(x)sum(x=="RES",na.rm=T)>=1),]$a_lab_id

human_ast$reserve=ifelse(human_ast$a_lab_id%in%res_id,"RES","SUS")
sus_id=reserve_list[apply(reserve_list, 1, function(x)sum(x=="RES",na.rm=T)==0),]$a_lab_id
new_df2=data.frame(PID=human_ast$i_pid,lab_id=human_ast$a_lab_id,MRSA=human_ast$MRSA,MDR=human_ast$resistance,Reserve)

new_df2=human_ast%>%
  select(PID=i_pid,lab_id=a_lab_id,MRSA=MRSA,MDR=resistance,Reserve=reserve,amocla:vancomycin)
new_df2$PID=substring(new_df2$PID,1,5)
substring(new_df2$PID
#--------------------------------------------------------------------------
merge_data=right_join(new_df,new_df2,by="PID")
library(xlsx)
write.xlsx(merge_data,file="C:\\Users\\Danwei Yao\\Desktop\\Sequence_data.xlsx",row.names = T,showNA = F)
