library(readxl)
library(dplyr)
library(tidyverse)
question<-read_excel("~/Desktop/AMR/Questionnaire_20210326.xlsx",sheet = 1)
head(question)

isolates<-read.csv("~/Desktop/isolated.csv",sep=",",header=T)
dim(isolates)
colnames(isolates)[2]
# human_isoalted: nasal----------
iso_human<-isolates%>%
  filter(grepl("^C.*N$",i_pid))

nasal<-ast%>%
  filter(grepl("^93",a_lab_id))%>%
  select(1:5,contains("result"))

#baseline sociology  datat------------------------------------------
#https://slcladal.github.io/surveys.html
sociology<-question%>%
dplyr::select(PID,age_BL:occup_housewife_BL,num_household_BL,num_child_BL,num_eld_BL,house_occup_cook_BL:house_occup_farmer_BL)
str(sociology)
sociology$age_BL<-as.numeric(sociology$age_BL)
sociology$edu_level_BL<-as.numeric(sociology$edu_level_BL)
sociology$num_child_BL<-as.numeric(sociology$num_child_BL)
sociology$num_household_BL<-as.numeric(sociology$num_household_BL)

#--------------------------------------------------------------------------------------
isoid=unique(iso_human$i_pid)
# a for isolated id
a=substring(isoid,1,5)
b=substring(unique(nasal$i_pid),1,5)

c=substring(unique(not_yet$i_pid),1,5)
# sum 
d=unique(question$PID)


tested=c(b,a)
combine_id=c(b,a,c)
left=setdiff(d,tested)
not_sau=setdiff(left,c)


not_sau2=d[!(d%in%combine_id)]
length(not_sau2)
s_aurs=substring(unique(subset(nasal,a_oxacillin_result=="SUS")$i_pid),1,5)
length(s_aurs)
mrsa=substring(unique(subset(nasal,a_oxacillin_result=="RES")$i_pid),1,5)

others=intersect(s_aurs,mrsa)
new_s_aurs=s_aurs[!(s_aurs%in%others)]
length(new_s_aurs)

#-------------------------data -----------------------------------------------------------
not_yet<-read.csv("~/Desktop/not_started.csv",sep=",",header=T)
not_yet<-not_yet%>%
  filter(grepl("^C.*1N$",i_pid))
unique(not_yet$i_pid)


nasal2=ast%>%
  filter(grepl("^C.*1N",i_pid))%>%
  summarise(n=n_distinct(i_pid))
dim(question)
#-data prepareation----------------------------------------------------------------
groupA=question[question$PID%in%not_sau,]
groupA=cbind(groupA,group="A")
colnames(groupA)
groupB=question[question$PID%in%s_aurs,]
groupB=cbind(groupB,group="B")

groupC=question[question$PID%in%mrsa,]
groupC=cbind(groupC,group="C")

groups=rbind(groupA,groupB,groupC)

#----------------------------------------------------------------------------------

socigroup=groups%>%
dplyr::select(PID,gender_BL,age_BL:occup_housewife_BL,household_income_BL,num_household_BL,num_child_BL,num_eld_BL,group)
str(socigroup)
which(colSums(socigroup[,5:12],na.rm = T)!=0)
socigroup=socigroup%>%
  select(PID,gender_BL,age_BL,edu_level_BL,occup_student_BL,occup_med_BL,occup_housewife_BL,household_income_BL,num_household_BL,num_child_BL,num_eld_BL,group)
socigroup$age_BL=as.numeric(socigroup$age_BL)
#socigroup[,c(3,4,8:11)]=lapply(socigroup[,c(3,4,8:11)],as.numeric)
socigroup[,4:12]=lapply(socigroup[,4:12], factor)
levels(socigroup$gender_BL)<-c("Male","Female","Missing")
levels(socigroup$num_household_BL)<-c("1","2-3","4-5","more than 6")
levels(socigroup$edu_level_BL)<-c("Primary or Below","Lower secondary","Upper secondary","Teritary or above")
levels(socigroup$household_income_BL)<-c("Less than $10,000","$10,000-19,999","$20,000-29,999","$30,000-39,999","$40,000-49,999","50,000-59,999","More than 60,000","Not sure","Prefer not to answer")
levels(socigroup$group)<-c("No S.aureus isolated","S.aureus SUS","MRSA")
str(socigroup)
attr(socigroup$gender_BL,"label")<-"Gender"
attr(socigroup$age_BL,"label")<-"Age"
attr(socigroup$edu_level_BL,"label")<-"Education Level"
attr(socigroup$occup_student_BL,"label")<-"Occupation:student?"
attr(socigroup$occup_med_BL,"label")<-"Occupation:medical professionals?"
attr(socigroup$occup_housewife_BL,"label")<-"Occupation:housewife/husband?"
attr(socigroup$num_household_BL,"label")<-"How many household members?"
attr(socigroup$num_child_BL,"label")<-"How many children aged 12 or below?"

attr(socigroup$num_eld_BL,"label")<-"How many elderies aged 65 or above?"
attr(socigroup$household_income_BL,"label")<-"Total monthly household income?"

socigroup[which(socigroup$gender_BL=="NA"),]

#for lifestyle analysis-----------------------------------------------------------
colnames(groups)
lifestyle=groups%>%
  dplyr::select(PID,veg_BL,where_buy_food_BL,handle_raw_BL,raw_seafood_BL,raw_veg_BL,swim_BL,group)
str(lifestyle)

lifestyle[,2:8]=lapply(lifestyle[,2:8], factor)
attr(lifestyle$veg_BL,"label")<-"Are you  a vegetarian?"
attr(lifestyle$where_buy_food_BL,"label")<-"Where to buy food materials?"
attr(lifestyle$handle_raw_BL,"label")<-"How often  Handle raw food materials_1 week?"
attr(lifestyle$raw_seafood_BL,"label")<-"How often eat uncooked seafood_1 week?"
attr(lifestyle$raw_veg_BL,"label")<-"How often eat uncooked vegetable_1 week?"
attr(lifestyle$swim_BL,"label")<-"How many times water activities_1 year?"
# for labling levels
levels(lifestyle$group)<-c("No S.aureus isolated","S.aureus SUS","MRSA")
levels(lifestyle$where_buy_food_BL)<-c("Wet maket","Super market","Both","Unknown")
levels(lifestyle$handle_raw_BL)<-c("Never","1-2 times","3-4 times","5-6 times","more than  7 times","Unknown")
levels(lifestyle$raw_seafood_BL)<-c("Never","1-2 times","3-4 times","5-6 times","more than  7 times")
levels(lifestyle$raw_veg_BL)<-c("Never","1-2 times","3-4 times","5-6 times","more than  7 times","Unknown")
levels(lifestyle$swim_BL)<-c("Never","1-2 times","3-4 times","5-6 times","more than  7 times","Unknown")

# for knowledge about  antibiotics---------------------------------------------------------------------
knowledge=groups%>%
  select(PID,ABX_kill_virus_BL:ABR_spread_BL,group)
str(knowledge)
dim(knowledge)
knowledge[,2:29]=lapply(knowledge[,2:29], as.numeric)
 knowledge[,2:30]=lapply(knowledge[,2:30], factor)
levels(knowledge$group)<-c("Without an S.aureus","S.aureus SUS","MRSA")
#for use of antibiotics --------------------------------------------------------------------------------
usage<-groups%>%
  select(PID,consult_gp_year_BL,ever_taken_ABX_BL,course_year_BL,side_headache_BL:side_others_00,why_keep_1_BL:why_keep_5_BL,group)
dim(usage)

# for health status-------------------------------------------------------------------------------
library(tidyverse)
colnames(groups)
health=groups%>%
  select(PID,infect_amrd_BL:others_BL,meds_BL,probiotics_BL:tb_BL,tb_6m_BL,tb_recover_BL,group)%>%
  replace_na("unknown")
health[,2:29]<-lapply(health[,2:29],as.numeric)
health[,2:30]<-lapply(health[,2:30],factor)
dim(health)
str(health) 
levels(health$group)<-c("Without an S.aureus","Non-MRSA","MRSA")
mrsaid=unique(groupC$PID)
head(question)

table_mrsa=subset(question,PID%in%mrsaid)%>%
  select(district_recruit_BL)


