#use data from ast result and questionnaire 
dim(ast_319)
dim(ques05)
library(dplyr)
library(tidyverse)
#-----------------------------------------------------------------
human.Mic<-function(humandata){
  datcount<-humandata
  datcount[,7:41]=lapply(datcount[,7:41],as.numeric)
  dat=datcount%>%
    select_if(~!is.numeric(.)||sum(.)!=0)
  example2=dat[,7:dim(dat)[2]]
  example2[,1:dim(example2)[2]]=lapply(seq(1,dim(example2)[2],by=1), function(x)example2[,x]=example2[,x]/cut_off[x])
  example2<-cbind(sample=datcount$i_pid,id=datcount$a_lab_id,example2)
  example2<-gather(example2,a_coamoxiclav_mic:a_cotrimoxazole_mic,key="colname",value = "value")
  example2$sample=substring(example2$sample,1,5)
  example2$colname=sapply(strsplit(example2$colname,"_"),function(x)x[2])
  example2$colname=factor(example2$colname,levels = c("coamoxiclav","ampicillin","Chlora.phenicol",
                                                      "cotrimoxazole","tetracyclines","gentamicin","cefoxitin","ceftazidime",
                                                      "ceftriaxone", "cefepime","ciprofloxacin","tobramycin","imipenem",
                                                      "merope","azithromycin","aztreonam","colistin"))
  return(example2)
}
human_june=ast_319%>%
  filter(grepl("^C.*S$",i_pid)&a_bacteria=="ECOLI")%>%
  select(!contains("result"))

human_june=data.frame(lapply(human_june, function(x){gsub("<= |< |> ","",x)}))
dim(human_june)


parti_mic=human.Mic(human_june)

#---------Group people----------------------------------------------
table.dat # from monthly questionnaire.R 
glimpse(table.dat)
table(table.dat$group,table.dat$course_year_BL)
table.dat$course_year_BL
table.dat=table.dat%>%
  group_by(group)%>%
  mutate(abx_group=case_when(course_year_BL==0~"Didn't use",
                             course_year_BL==1|2|3|4~"Use Abx",
                            course_year_BL==2|3|4~"use 3+ times",
                            TRUE ~ as.character(NA)
                            
                             ))
#---categorize into three groups----------------------------------------------------------
table.dat=table.dat%>%
  group_by(group)%>%
   mutate(abx_group=case_when(course_year_BL==0~"Didn't use",
                                                            course_year_BL==1~"use 1-2 times",
                                                           course_year_BL==2|3|4~"use 3+ times",
                                                            TRUE ~ as.character(NA)
                                                           
                                                             ))

# grouped by se x+ grid 
drug_data=table.dat%>%
  select(PID,group,abx_group)

drug_map=right_join(drug_data,parti_mic,by=c("PID"="sample"))

cat("---change the font size for human map--------------")
plot.heatmap<-function(data){
  data%>%
    ggplot(aes(x=colname,y=id))+
    geom_tile(aes(fill=cut(value,breaks=c(0,1,4,8,Inf),labels=c("Susceptible","Intermediate","Resistance","Heteroresistance"))),size=0.2,color="white")+
    scale_fill_manual(values=c("#ddf1da","#abdda4","#fdae61","#d53e4f"),na.value="grey90",name="Resistance level")+
    scale_x_discrete(guide = guide_axis(angle = 90))+
    theme_grey(base_size = 10)+
    xlab("Antibiotics")+ylab("Isolate Id")+
    theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                             family="Helvetica",size=18),
          text=element_text(family="Times New Roman",size=18),
          axis.text.y=element_text(size=9,
                                   hjust=1,vjust=0.2,color='grey40'),
          axis.text.x=element_text(size=18))
  
  
}

p1=plot.heatmap(mapa)+ggtitle("Male(<65)")
p2=plot.heatmap(mapb)+ggtitle("Male(65+)")
p3=plot.heatmap(mapc)+ggtitle("Female(<65)")
p4=plot.heatmap(mapd)+ggtitle("Female(65+)")
# make it a dark panel
grid_map<-function(data,datagroup){
data%>%
  filter(group==datagroup)%>%
  ggplot(aes(x=colname,y=id))+
  geom_tile(aes(fill=cut(value,breaks=c(0,1,4,8,Inf),labels=c("Susceptible","Intermediate","Resistance","Heteroresistance"))))+
  scale_fill_manual(values=c("#ddf1da","#abdda4","#fdae61","#d53e4f"),na.value="grey90",name="Resistance level")+
  #scale_fill_gradient2(low = "white", mid="lightyellow",high = "red", midpoint=1)+
  #scale_fill_gradientn(colors = colors, breaks = b, labels = format(b))+
  #sscale_fill_gradientn(name="CPU Utilization", colours=pals,  
  #values=vals,limits=c(0,100), breaks = brk)
  scale_x_discrete(guide = guide_axis(angle = 90))+
  facet_wrap(~abx_group,ncol=1,scales = "free_y"
  )+
    theme_grey(base_size = 10)+ #theme_tufte(base_family="Times New Roman")+
  xlab("Antibiotics")+ylab("Isolate Id")+
  theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                           family="Times New Roman",size=16,color = "white"),
        text=element_text(family="Times New Roman",size=16,color = "white"),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=18,color = "white"),
        plot.title=element_text(hjust=0),
axis.ticks=element_blank(),
panel.background = element_rect(color = "#242B47", fill = "#242B47"),
plot.background = element_rect(color = "#242B47", fill = "#242B47"),
strip.background = element_rect(color = "#242B47", fill = "#242B47"),
strip.text = element_text(hjust=0,size=16,color = "white", family = "Georgia"),
panel.grid.major.x = element_line(color = "grey48", size = 0.05),
panel.grid.minor.x = element_blank(),
panel.grid.major.y = element_blank(),
legend.position = 'none')
#panel.border=element_blank())
}
ex1=drug_map%>%
  filter(group=="Female(<65)")

m2=grid_map(drug_map,"Male(65+)")+ggtitle("Male(65+)")
m3=grid_map(drug_map,"Female(<65)")+ggtitle("Female(<65)")
m4=grid_map(drug_map,"Female(65+)")+ggtitle("Female(65+)")

ggarrange(m1+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),
                   axis.title.x=element_blank()),
          m3+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),
                   axis.title.x=element_blank()),
          m2,m4,
          ncol=2,nrow=2,
          heights = c(1,1,1,1))

ggarrange(m1,m2,m3,m4,
          ncol=4,nrow=1,
         heights = c(1,1,1,1))
ggsave("C:\\Users\\Danwei Yao\\Desktop\\Abx&gender_shallow_v7.png",
       width =50,height = 45,units = "cm",dpi=500,type="cairo-png") 
 library(ggthemes)

#-------------------with seperate factes of antibiotic use frequency ------------------------;
grid_map_white<-function(data,datagroup){
  data%>%
    filter(group==datagroup)%>%
    ggplot(aes(x=colname,y=id))+
    geom_tile(aes(fill=cut(value,breaks=c(0,1,4,8,Inf),labels=c("Susceptible","Intermediate","Resistance","Heteroresistance"))))+
    scale_fill_manual(values=c("#ddf1da","#abdda4","#fdae61","#d53e4f"),na.value="grey90",name="Resistance level")+
  
    scale_x_discrete(guide = guide_axis(angle = 90))+
    facet_wrap(~abx_group,ncol=1,scales = "free_y"
    )+
    theme_grey(base_size = 10)+ #theme_tufte(base_family="Times New Roman")+
    xlab("Antibiotics")+ylab("Isolate Id")+
    theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                             family="Times New Roman",size=16),
          text=element_text(family="Times New Roman",size=16),
          axis.text.y=element_blank(),
          axis.text.x=element_text(size=18),
          plot.title=element_text(hjust=0),
          axis.ticks=element_blank(),
          panel.grid.major.x = element_line(color = "grey48", size = 0.05),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          strip.text = element_text(hjust=0,size=16),
          strip.background = element_rect(color = "white", fill = "white"))
          
  #panel.border=element_blank())
}
m1=grid_map_white(drug_map,datagroup = "Male(<65)")+ggtitle("Male(<65)")
m2=grid_map_white(drug_map,"Male(65+)")+ggtitle("Male(65+)")
m3=grid_map_white(drug_map,"Female(<65)")+ggtitle("Female(<65)")
m4=grid_map_white(drug_map,"Female(65+)")+ggtitle("Female(65+)")
ggarrange(m1,m2,m3,m4,
          ncol=4,nrow=1,
          heights = c(1,1,1,1),common.legend = T,legend = "bottom")
drug_data%>% 
  group_by(group,abx_group)%>%summarise(n())
table(drug_data$group,drug_data$abx_group)  
