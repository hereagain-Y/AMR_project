# frist table group by follow-up times 
library(readxl)
library(dplyr)
ast=read_excel("AST_0721.xlsx"
)

qustion=read_excel("Questionnaire_20210716.xlsx"
)

sequence_ast=ast
people=substring(unique(sequence_ast$i_pid),1,5)
ast_ques=qustion%>%
  filter(PID%in%people)


ast_ques$house_part1_BL

# with 
df1=ast_ques%>%
  filter(house_part1_BL!="NA")%>%
  dplyr::select(PID,house_part1_BL:house_part5_BL)
# remove () in every column
df1[,2:6]<-sapply(df1[,2:6],function(x)gsub("\\(.*\\)","",x))

# substreing the Pid
testdata=as.data.frame(apply(df1,2,function(x)substring(x,2,5)))
testdata$max_id=as.vector(apply(testdata,1,max))
class(testdata$house_part1_BL)


group_id <- testdata %>% group_indices(max_id) 

testdata=testdata %>%
  mutate(household=paste("house_hold",group_id,""))
testdata$max_id=gsub(" ", "", testdata$max_id, fixed = TRUE)
# add a house hold column 
ast_ques$new_house=ifelse(ast_ques$house_part1_BL=='NA',"no",testdata$household)

new_df=data.frame(PID=ast_ques$PID,Age=ast_ques$age_BL,Gender=ast_ques$gender_BL,Household=ast_ques$new_house,Use_abx=ast_ques$course_year_BL,Ever_use=ast_ques$ever_taken_ABX_BL)
new_df$id=paste(new_df$PID,"BL",sep="_")
#---------------------------------------------------------------------------
# Follow up 1 data ---------------------------------------------------------
f1_dat<-ast_ques%>%
  filter(bl_to_fu1_complete=="2")

f1_dat$id=paste(f1_dat$PID,"F1",sep="_")


new_df2=data.frame(PID=f1_dat$PID,Age=f1_dat$age_BL,Gender=f1_dat$gender_BL,Household=f1_dat$new_house,Use_abx=f1_dat$course_month_FU1,Ever_use=f1_dat$ever_taken_ABX_BL,id=f1_dat$id)
#-------------------------------------------------
FU_data<- function(dataset,followterm,times,colname){
  follow_up=dataset%>%
    filter((!!as.name(followterm))=="2")
  follow_up$id=paste(follow_up$PID,times,sep="_")
  new_data=data.frame(PID=follow_up$PID,Age=follow_up$age_BL,Gender=follow_up$gender_BL,Household=follow_up$new_house,Use_abx=follow_up[[colname]],Ever_use=follow_up$ever_taken_ABX_BL,id=follow_up$id)
  new_data
}
new_df2=FU_data(ast_ques,'bl_to_fu1_complete','F1','course_month_FU1')
new_df3=FU_data(ast_ques,'fu1_to_fu2_complete','F2','course_month_FU2')
new_df4=FU_data(ast_ques,'fu2_to_fu3_complete','F3','course_month_FU3')
new_df5=FU_data(ast_ques,'fu3_to_fu4_complete','F4','course_month_FU4')

follow_ques=rbind(new_df,new_df2,new_df3,new_df4,new_df5)

#---------------------------------------------------------
cat("--------------for MDR and MRSA-------------")

MSSA=ast%>%
  filter(grepl("^C.*N$",i_pid)&a_oxacillin_result=="SUS")%>%
  dplyr::select(i_pid, a_lab_id)
MRSA=ast%>%
  filter(grepl("^C.*N$",i_pid)&a_oxacillin_result=="RES")%>%
  dplyr::select(i_pid, a_lab_id)
mssa_id=MSSA$a_lab_id
mrsa_id=MRSA$a_lab_id

human_ast$MRSA=ifelse(human_ast$a_lab_id%in%mssa_id,"MSSA",
                      ifelse(human_ast$a_lab_id%in%mrsa_id,"MRSA",
                             "No"))

#-------------------------------------------
human_ast=ast%>%
  filter(grepl("^C.*",i_pid))%>%
  dplyr::  select(1:5,contains("result"))
#------------------------------------------

#count MDR frquency
#
bacteria=human_ast%>%
  #filter(a_bacteria=="ECOLI")%>%
  select_if(~!is.numeric(.)||sum(.)!=0)
glimpse(ecoli)
colnames(human_ast)[6:40]=sapply(strsplit(colnames(human_ast)[6:40],"_"),function(x)x[2])


lact1=human_ast%>%
  dplyr:: select(1:2,all_of(lactams))
human_ast$ampicillin

human_ast[apply(human_ast, 1, function(x)any(x!="SUS")),]$a_lab_id# any not susceptible considered 
lactams=c("amocla")
penicllin=c("ampicillin")
cephalosporin=c("cefoxitin","ceftazidime","ceftriaxone","cefepime")
#cefepimepi: 4th generation cephalosporin antibiotic.
#cefoxition, foxscr: 2nd
#ceftazidime: 3rd 
#ceftriaxone: 3rd;
Tetracycline=c("tetracyclines")
quinolones=c("ciprofloxacin")
aminoglycosides=c("gentamicin","tobramycin")
macrolides=c("azithromycin")
carbapenems=c("merope","imipenem")
sulfonamide=c("trisul")
phenicol=c("chlora")
rifamycins=c("rifampin")
#uncoventional=c("trisul","chlora","clindamycin","nitrofurantoin","rifampin")
reserve=c("colistin","aztreonam")
lincosamides=c("clindamycin")
nitro=c("nitrofurantoin")

category_list=list(lactams,macrolides,aminoglycosides,quinolones,carbapenems,
                   Tetracycline,rifamycins,sulfonamide,phenicol,lincosamides,nitro,reserve)
length(category_list)


mdr_id=sapply(1:12, function(x){
  data=human_ast[,c("i_pid","a_lab_id",category_list[[x]])]
  id=unique(data[apply(data, 1, function(x)any(x=="RES"|x=="INT")),]$a_lab_id)
  id
})


mdr_id=unlist(mdr_id)
mdr_id=mdr_id[!is.na(mdr_id)]
count_frquency<-function(variable,thresh,endpoint){
  ind<-ave(rep(1,length(variable)),variable,FUN=length)
  variable[ind>thresh&ind<endpoint]
}


unique(as.data.table(mdr_id)[, N := .N, by = mdr_id][N > 0&N<3]$diff_Ids)

# data.table(mdr_id)[, .N, mdr_id] count the times of duplicate
No_mdr=unique(count_frquency(mdr_id,0,3))#<=3
Mdr= unique(count_frquency(mdr_id,2,7))#<=3
high_Mdr= unique(count_frquency(mdr_id,6,8))# high mdr
human_ast$resistance=ifelse(human_ast$a_lab_id%in%No_mdr,"No MDR",
                            ifelse(human_ast$a_lab_id%in%Mdr,"MDR",
                                   ifelse(human_ast$a_lab_id%in%high_Mdr,"High MDR","All SUS")))
library(data.table)
resistance_count=data.table(mdr_id)[, .N, mdr_id]
human_ast$resistance_length=ifelse(human_ast$a_lab_id%in%resistance_count$mdr_id,resistance_count$N,0)
#--------------
#reserve

reserve_list=human_ast%>%
  dplyr::select(1:2,all_of(reserve))
res_id=reserve_list[apply(reserve_list, 1, function(x)sum(x=="RES"|x=="INT",na.rm=T)>=1),]$a_lab_id


human_ast$reserve=ifelse(human_ast$a_lab_id%in%res_id,"RES or I","SUS")
library(stringi)

ast_df=data.frame(PID=human_ast$i_pid,lab_id=human_ast$a_lab_id,MRSA=human_ast$MRSA,MDR=human_ast$resistance, MDR_length=human_ast$resistance_length,Reserve=human_ast$reserve)
oder_id=stri_sub(human_ast$i_pid,-2,-2)
human_ast$follow<-ifelse(oder_id=="1","BL",
                    ifelse(oder_id=="2","F1",
                           ifelse(oder_id=="3","F2",
                                  ifelse(oder_id=="4","F3","0"))))
human_ast$id=paste(substring(human_ast$i_pid,1,5),human_ast$follow,sep="_")

ast_df2=human_ast%>%
  dplyr::select(id=id,lab_id=a_lab_id,MRSA=MRSA,MDR=resistance,MDR_length=resistance_length,reserve,follow_up=follow,amocla:vancomycin)


#--------------------------------------------------------------------------
merge_data=right_join(follow_ques,ast_df2,by="id")
merge_data=merge_data%>%
  relocate(lab_id,.after = PID)%>%
  relocate(follow_up,.after = Gender)%>%
  relocate(id,.after = PID)
library(xlsx)
write.xlsx(merge_data,file="C:\\Users\\Danwei Yao\\Desktop\\Sequence_data0721.xlsx",row.names = T,showNA = F)
