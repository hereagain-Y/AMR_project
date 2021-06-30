#' @title
#'Monthly routine for questionnaire data
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
#' 
#' do_chisq_test(table.dat,"edu_level_BL","group","1")
#' export2word(sum_tab,file = "C:\\Users\\Danwei Yao\\Desktop\\Answer_table0618.docx",strip = T)
#'
#' 


library(readxl)
library(dplyr)
library(tidyverse)
library(sqldf)


#---------Do certain chisq test for male and Female groups seperately 
# do chisq-test for ceterin variables 
	
do_chisq_test=function(data,var1=character(),var2=character(),levels,gender){
  if(gender=="1"){ # if male
  
    cat("this is fro male comparison")
    return(sapply(1:levels, function(x)chisq.test(table(data[[var1]],data[[var2]])[x,1:2])$p.val))
  }else{# if female 
   
    cat("this is for female comparison")
    return(sapply(1:levels, function(x)chisq.test(table(data[[var1]],data[[var2]])[x,3:4])$p.val))
  }
}
 #example
		  do_chisq_test(table.dat,"edu_level_BL","group","1")
		  

Dostats.test=function(data,var1=character(),group=character(),test){
  	if(test=="chisq"){
   			 result1=chisq.test(table(data[[var1]],data[[group]])[,c(1,3)])$p.val# same age comparing male and female
    			result2=chisq.test(table(data[[var1]],data[[group]])[,c(2,4)])$p.val
    			
   
  }else{
     		 result1=fisher.test(table(data[[var1]],data[[group]])[,c(1,3)])$p.val
     		 result2=fisher.test(table(data[[var1]],data[[group]])[,c(2,4)])$p.val
     			
      
  }
	 rsult_table=cbind("less than 65"=result1,
                         	 "more than 65"=result2)
  return(rsult_table)
}

# ------------------Table 1------------------------------------------------
#-------------------------------------------------------------------------
		  
pvalue<-function(x,...){
  y<-unlist(x)
  g<-factor(rep(1:length(x),times=sapply(x,length)))
  if(is.numeric(y)){
    p<-t.test(y~g)$p.value
  }else if(!is.numeric(y)&all(table(y,g)>5)){
    
    p<-chisq.test(table(y,g))$p.value
  }else{
    p<-fisher.test(table(y,g))$p.value
  }
c(" ", sub("<","&lt;",format.pval(p,digits = 2,eps=0.001)))#replace "" with p-val

}

		  # add a p value column in table1 
subsetdat=table.dat[table.dat$group=="Male(<65)"|table.dat$group=="Female(<65)",]

table1(~age_BL+edu_level_BL+occup_student_BL+num_child_BL+num_household_BL+AMR_BL|group,data=subsetdat,overall = F,extra.col = list('P_value'=pvalue))
		  
# ------------------------------------------------------------------------*
#--------------------Data Clean-------------------------------------------*
#-------------------------------------------------------------------------*
ques05=read_excel("C:\\Users\\Danwei Yao\\Desktop\\Questionnaire_20210618.xlsx")

table.dat=ques05%>%
  select(PID, age_BL:occup_others_BL, district_living_BL,household_income_BL,num_household_BL,num_child_BL,num_eld_BL,
         infect_amrd_BL:others_BL,consult_gp_year_BL:others_hosp_clin_BL,
         meds_BL,probiotics_BL,tb_BL,tb_ppl_BL,veg_BL,buy_ABXF_BL,ABX_kill_virus_BL:ABR_bacteria_BL,
         ever_taken_ABX_BL,course_year_BL,last_ABX_BL,how_get_ABX_BL,complete_course_BL,keep_ABX_BL
  )


#-------------------------------------------------------------------------------
table.dat=table.dat[complete.cases(table.dat),]
 table.dat[,2:73]=lapply(table.dat[,2:73],as.numeric)

# Group by age 
table.dat$group=ifelse(table.dat$gender_BL==1&table.dat$age_BL<65,1,
                       ifelse(table.dat$gender_BL==1&table.dat$age_BL>=65,2,
                              ifelse(table.dat$gender_BL==2&table.dat$age_BL<65,3,
                                     ifelse(!is.na(table.dat$age_BL)&!is.na(table.dat$gender_BL),4,5))))# regroup the people by their gender and age 


	table.dat$group=factor(table.dat$group,levels = c(1,2,3,4),labels = c("Male(<65)","Male(65+)","Female(<65)","Female(65+)"))

#find the positions
	match("diabetes_BL",names(table.dat))
	match("others_BL",names(table.dat))
# if any yes  for chornic diease
t1=subset(table.dat,rowSums(table.dat[,23:33])==0)# create a new col for chornic disease
	number_disease=rowSums(table.dat[,23:33],na.rm = T)
	table.dat$number_disease=cut(number_disease,c(-Inf,0,2,Inf),labels = c("No","1-2","2+"))

t1%>%
  group_by(group)%>%
  summarise(count=n())

table.dat$condition=ifelse(rowSums(table.dat[,23:33])==0,"no","yes")

#turn in to a factor to reduce NA

	table.dat[,3:75]=lapply(table.dat[,3:75],factor)


  
#make a table 1 
	library(compareGroups)
	tab=compareGroups(group~.,data=table.dat)
	sum_tab=createTable(tab,hide.no = "0")
	export2word(sum_tab,file = "C:\\Users\\Danwei Yao\\Desktop\\Answer_table0618.docx",strip = T)
	
