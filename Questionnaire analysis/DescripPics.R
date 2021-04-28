#Multilevel analysis for antibiogram length among PIDs---------------------------
# load libraries
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

  dim(human_stool)  

#combine two dataset together 

 library("lamisc")
  
  dupe_list=human_stool%>%select(i_pid,a_amocla_result:a_vancomycin_result)%>%
    group_by(i_pid)%>%
    count_duplicates()%>%select(i_pid,dupe_count)
  
  
  grouplist=human_stool%>%select(i_pid,a_lab_id)%>%
    group_by(i_pid)%>%
    summarise(count=n())
    
shit1=left_join(dupe_list,grouplist,by="i_pid")
shit1$check<-ifelse(shit1$dupe_count==shit1$count,0,1)
# exclude duplicate lines 
shit1=shit1%>%select(i_pid,check)%>%
  distinct(i_pid, .keep_all = TRUE)

homoIDs=shit1[which(shit1$check==0),]$i_pid
heterIDS=shit1$i_pid[!(shit1$i_pid %in% homoIDs)]

# cal the antibiogram ;ength for each sample
n_resistance.count=function(x){
  result_table=cbind(RES_n=sum(x=="RES",na.rm = T))#SUS=b)
  return(result_table)
}

antibiolength.count=function(data){
  human_res_count<-t(t(apply(data[,-c(1:5)],MARGIN = 1,function(x)n_resistance.count(x))))
  human_res_count=as.data.frame(human_res_count)
  colnames(human_res_count)=c("RES")
  human_res_count=cbind(id=data$i_pid,lab_id=data$a_lab_id,human_res_count)
  human_res_count
}
countable=antibiolength.count(human_stool)
# compute the final data 
humanstools=left_join(countable,shit1,by=c("id"="i_pid"))
file.choose()
humanstools$id=substring(humanstools$id,1,5)
joined_data=right_join(question,humanstools,by=c("PID"="id"))

table(joined_data$course_year_BL)
save(joined_data,file="C:\\Users\\Danwei Yao\\Desktop\\output\\ecoli.hetero.Rdata")
# multilevel models--------------------------------------------------------------
#part 1 discriptive analysis 
str(joined_data)
joined_data$check=factor(joined_data$check,levels=c(0,1),label=c("homoresistance","heteroresistance"))
joined_data$edu_level_BL=cut(joined_data$edu_level_BL,breaks=c(-Inf,3,4),labels=c("secondary (S4-S6)/Matriculation or below","Tertiary or above"))
joined_data$num_eld_BL=cut(joined_data$num_eld_BL,breaks = c(-Inf,0,1,Inf),labels = c("None","1","2+"))
joined_data$age_BL<- cut(joined_data$age_BL, 
                   breaks = c(-Inf, 18, 45, 65, Inf), 
                   labels = c("<18", "18-44", "45-64", "65+"), 
                   right = FALSE)
joined_data$handle_raw_BL=cut(joined_data$handle_raw_BL, 
                              breaks = c(-Inf, 0, 2, Inf), 
                              labels = c("Never", "1-4times", "more than 4 times")
                              )
joined_data$raw_veg_BL=cut(joined_data$raw_veg_BL, 
                              breaks = c(-Inf, 0, 2, Inf), 
                              labels = c("Never", "1-4times", "more than 4 times")
)

joined_data$raw_seafood_BL=cut(joined_data$raw_seafood_BL, 
                           breaks = c(-Inf, 0, 2, Inf), 
                           labels = c("Never", "1-4times", "more than 4 times")
)
joined_data$swim_BL=cut(joined_data$swim_BL, 
                        breaks = c(-Inf, 0, 2, Inf), 
                        labels = c("Never", "1-4times", "more than 4 times"))

joined_data$consult_gp_year_BL=cut(joined_data$consult_gp_year_BL, 
                        breaks = c(-Inf, 0, 1, 2,Inf), 
                        labels = c("Never", "1-3times", "4-6 times","more than 6 times"))


joined_data$course_year_BL=cut(joined_data$course_year_BL, 
                                   breaks = c(-Inf, 0, 2, Inf), 
                                   labels = c("Never", "1-4 times", "more than 4 times"))



dataset=joined_data%>%
  select(PID,lab_id:check,gender_BL,age_BL,edu_level_BL,num_eld_BL,consult_gp_year_BL,ever_taken_ABX_BL,course_year_BL,
         diabetes_BL:asthma_BL,
         handle_raw_BL:swim_BL)
str(dataset)
dim(dataset)
dataset[,4:25]=lapply(dataset[,4:25],factor)
# draw plots
library(ggstatsplot)
ggbetweenstats(x=check,y=RES,data=dataset,
               xlab="Whether heteroresistance",ylab="Antibiogram length",
               pairwise.comparisons = TRUE,p.adjust.method = "fdr", 
               ggtheme = ggplot2::theme_gray())

ggbetweenstats(x=num_eld_BL,y=RES,data=dataset,
               xlab="Number of elderes at home",ylab="Antibiogram length",
               pairwise.comparisons = TRUE,p.adjust.method = "fdr",
               ggtheme = ggplot2::theme_gray())


ggbetweenstats(x=raw_seafood_BL,y=RES,data=dataset,
               xlab="Times of eating raw seafood",ylab="Antibiogram length",
               pairwise.comparisons = TRUE,p.adjust.method = "fdr",
               ggtheme = ggplot2::theme_gray())





library(gapminder)
ggstatsplot::grouped_ggbetweenstats(
  data=dataset,x=handle_raw_BL,y=RES,grouping.var = check,
  #title.prefix="Type of resistance:",
  messages = FALSE,
  # arguments relevant for ggstatsplot::combine_plots
  title.text = "Antibiogram length among people with diffrent food handle frequencies",
  nrow = 2,
  ncol = 1,
  labels = c("(a)","(b)")
)
p2 <- ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = dataset, age_BL == "18-44"),
  x = raw_veg_BL,
  y = RES,title = "Age:18-44",
  plot.type = "boxviolin",
  type = "np",
  ylab="Antibiogram length",
  messages = FALSE,pairwise.comparisons = TRUE,
  ggtheme = ggplot2::theme_gray())
 p3=ggstatsplot::ggbetweenstats(
  data = dplyr::filter(.data = dataset, age_BL == "45-64"),
  x = raw_veg_BL,
  y = RES,title = "Age:45-64",
  ylab="Antibiogram length",
  plot.type = "boxviolin",
  type="np",
  messages = FALSE,pairwise.comparisons = TRUE,
  ggtheme = ggplot2::theme_gray())
 p4=ggstatsplot::ggbetweenstats(
   data = dplyr::filter(.data = dataset, age_BL == "65+"),
   x = raw_veg_BL,
   y = RES,title = "Age:65+",
   ylab="Antibiogram length",
   plot.type = "boxviolin",
   type="np",pairwise.comparisons = TRUE,
   messages = FALSE,
   ggtheme = ggplot2::theme_gray())

 library(ggplot2)
 ggstatsplot::combine_plots(
   plotlist = list(p2,p3,p4), 
   plotgrid.args = list(nrow = 3, ncol = 1),
   labels = c("(a)", "(b)", "(c)"),
   title.text = "Comparison of raw vegetable frequency across age groups",
   #caption.text = "Note: Comparing results from parametric, non-parametric, and robust tests",
   title.size = 14
   #caption.size = 12
 )
 
 #-- sea food over age------------------------
 library(tidyverse)
 table(dataset$age_BL,dataset$raw_seafood_BL)
 # creating a list column with `ggstatsplot` plots
 p_list <-
   dataset %>%
   dplyr::filter(.data = ., age_BL != "<18") %>%
   dplyr::group_by(.data = ., age_BL) %>%
   dplyr::group_map(.f = ~ ggstatsplot::ggbetweenstats(data = ., x = raw_seafood_BL, y = RES,
      ylab="Antibiogram length" ,type = "np", pairwise.comparisons = TRUE,ggtheme = ggplot2::theme_gray() ))
 ggstatsplot::combine_plots(
   plotlist = p_list,
   annotation.args = list(tag_levels = list(paste0("Age:",as.vector(rlang::set_names(levels(as.factor(dataset$age_BL)))))),
                          title= "Frequency of sea food consuming over age groups"),
   plotgrid.args = list(nrow = 3, ncol = 1)                  
 ) 
 
 # food handle frequency over ages 
 table(dataset$age_BL,dataset$handle_raw_BL)
 p_list2 <-
   dataset %>%
   dplyr::filter(.data = ., age_BL != "<18") %>%
   dplyr::group_by(.data = ., age_BL) %>%
   dplyr::group_map(.f = ~ ggstatsplot::ggbetweenstats(data = ., x = handle_raw_BL, y = RES,
                                                       ylab="Antibiogram length" ,type = "np", pairwise.comparisons = TRUE,ggtheme = ggplot2::theme_gray() ))
 ggstatsplot::combine_plots(
   plotlist = p_list2,
   annotation.args = list(tag_levels = list(paste0("Age:",as.vector(rlang::set_names(levels(as.factor(dataset$age_BL)))))),
                          title= "Frequency of food handling over age groups"),
   plotgrid.args = list(nrow = 3, ncol = 1)                  
 ) 
 
a1=as.numeric(dataset$age_BL)
b1=as.numeric(dataset$cancer_BL)
c1=as.numeric(dataset)
cor.test(a1, b1)
cor.test(as.numeric(dataset$age_BL),as.numeric(dataset$ever_taken_ABX_BL))
dataset$ever_taken_ABX_BL
