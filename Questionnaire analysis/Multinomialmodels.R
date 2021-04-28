# load classified data called human_stool
(load("C:\\Users\\Danwei Yao\\Desktop\\output\\Defination_two.Rdata"))

# combine with questionnaire data
human_stool$id=substring(human_stool$i_pid,1,5)
head(human_stool)
new_data=human_stool%>%
  select(id,a_lab_id,newcheck)
joined_data2=right_join(question,new_data,by=c("PID"="id"))

dataset2=joined_data2%>%
  select(PID,a_lab_id,newcheck,gender_BL,age_BL,edu_level_BL,num_eld_BL,consult_gp_year_BL,ever_taken_ABX_BL,course_year_BL,
         diabetes_BL:asthma_BL,
         handle_raw_BL:swim_BL,last_ABX_BL)
table(dataset2$last_ABX_BL)
dataset2$newcheck=ifelse(dataset2$newcheck==3,2,dataset2$newcheck)
dim(dataset2)
# factor and rename the vars, grouped into ABX used and unused  
dim(dataset2)
used<-subset(dataset2,ever_taken_ABX_BL=="1")
notused<-subset(dataset2,ever_taken_ABX_BL=="0")
save(notused,file="C:\\Users\\Danwei Yao\\Desktop\\output\\Notused.Rdata")
dim(used)
length(unique(used$PID))
used$raw_veg_BL
table(notused$newcheck,notused$raw_seafood_BL)
used$consult_gp_year_BL=as.numeric(used$consult_gp_year_BL)
used$consult_gp_year_BL=cut(used$consult_gp_year_BL,breaks = c(-Inf,0,Inf),labels = c("Never","1+times"))
used$raw_veg_BL<-cut(used$raw_veg_BL,breaks=c(-Inf,1,Inf),labels = c("0-2times","3+times"))
dataset2$newcheck=factor(dataset2$newcheck,levels=c(0,1,2),label=c("homoresistance","25%heteroresistance","50%heteroresistance"))
table(dataset2$last_ABX_BL)
dataset2$raw_veg_BL<-as.numeric(dataset2$raw_veg_BL)
dataset2$edu_level_BL=cut(dataset2$edu_level_BL,breaks=c(-Inf,2,3,4),labels=c("S1-S3 or below","S4-S6/Matriculation","Tertiary or above"))
dataset2$num_eld_BL=cut(dataset2$num_eld_BL,breaks = c(-Inf,0,Inf),labels = c("None","1 or 2+"))

dataset2$handle_raw_BL=cut(dataset2$handle_raw_BL, 
                          breaks = c(-Inf, 0, 2, Inf), 
                          labels = c("Never", "1-4times", "more than 4 times")
)


dataset2$raw_seafood_BL=cut(dataset2$raw_seafood_BL, 
                           breaks = c(-Inf, 0, 1, Inf), 
                           labels = c("Never", "1-2times", "3-4 times")
)
dataset2$swim_BL=cut(dataset2$swim_BL, 
                    breaks = c(-Inf, 0, 1,Inf), 
                    labels = c("Never", "1-2 times", "2+ times"))

dataset2$consult_gp_year_BL=cut(dataset2$consult_gp_year_BL, 
                               breaks = c(-Inf, 0, 1,Inf), 
                               labels = c("Never", "1-3times", "4+ times"))


dataset2$course_year_BL=cut(dataset2$course_year_BL, 
                           breaks = c(-Inf, 0,Inf), 
                           labels = c("Never", "1+ times"))

dataset2$gender_BL=factor(dataset2$gender_BL,levels = c(1,2),labels = c("male","female"))
str(dataset2)
dataset2[,7:25]=lapply(dataset2[,7:25],factor)

dataset2.full=dataset2[complete.cases(dataset2),]
dim(dataset2.full)
dim(dataset2)
dataset2.full$course_year_BL
#####3#######################################################################
chisq.test(table(dataset2$gender_BL, dataset2$handle_raw_BL))# sig
chisq.test(table(dataset2$gender_BL, dataset2$raw_seafood_BL))

#########Univariate analysis########################################################
install.packages("nnet")
require(nnet)

# var selection

vars=c("age_BL","edu_level_BL","num_eld_BL" ,"consult_gp_year_BL" ,'course_year_BL',
       "handle_raw_BL","raw_seafood_BL"   ,"raw_veg_BL","swim_BL")
formlula=sapply(vars,function(x)as.formula(paste("newcheck~",x)))
univ_model=lapply(formlula, function(x){multinom(x,data=used)})
univ_result2=lapply(seq(univ_model), function(x){
  p_val<-list()
p_val<-lrtest(univ_model[[x]])
return(p_val)}
 
)
univ_result2
table(data3$newcheck,data3$course_year_BL)
#####################################################################
multinom.fit <- multinom(newcheck ~ course_year_BL, data = used)
summary(multinom.fit)
lrtest(multinom.fit)[[5]]
require(lmtest)
#############wrap a function to calculate the p value########################
likehoodmultinom_p <- function(model_lmm)
{
  
  i <- 1
  
  variables <-list()
  values <- list() 
  
  
  for (var in model_lmm$coefnames[-1]) { #exlcude intercept
    
    variables[[i]] =paste(var)# only include for one level vars 
    values[[i]]= lrtest(model_lmm)#[[5]]
    i=i+1
    ## Contributed to stack at: 
  }
  return (values)
}
likehoodmultinom_p(multinom.fit)
multinom.fit$coefnames
variable=paste(multinom.fit$coefnames[-1])
lrtest(multinom.fit)
########################################################

#multipl variable 
multifit2<-multinom(newcheck~age_BL+edu_level_BL+consult_gp_year_BL+handle_raw_BL+raw_veg_BL,data=used)
lrtest(multifit2)
summary(multifit2)
library(broom)
library(lmtest)

as.data.frame(tidy(multifit2))

library(gtsummary)

# regression output 
tbl_long <-
 multifit2%>%
  tbl_regression(exponentiate = TRUE) %>%
  modify_header(estimate ~ "**OR**")

install.packages("stargazer")

multifit3<-multinom(newcheck~edu_level_BL+num_eld_BL+
                    handle_raw_BL+
                      raw_veg_BL,data=train2)
summary(multifit3)
as.data.frame(tidy(multifit3))
z=summary(multifit2)$coefficients/summary(multifit2)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(multifit2))


 
 multifit3<-multinom(newcheck~edu_level_BL+num_eld_BL+
                       handle_raw_BL+
                       raw_veg_BL,data=data3)
# 
 save(used,file="C:\\Users\\Danwei Yao\\Desktop\\output\\Abxused.Rdata")
 # report
 library(devtools)
 install_github("holtzy/epuRate")
 library(epuRate)
 