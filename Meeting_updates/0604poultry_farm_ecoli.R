# title: generate meeting pics 
# '@ porvides with a clear understanding of our farm sampling process as well as the ast result.
# 'figure 1 is a summary of our monlthly collected sample
#' figure 2 describes the checked bacterial isolates in every farm 
#' figure 3 illustartes the Ast resistance patterns of animal farm samplesin the form of heatmap
#' figure 4 plots the waste ast pattern map



# load required libraries 
library(readxl)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(tidyverse)

#------------------------------------------------------------------------------------
#-Figure1----------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#load data
swine<-read_excel("C:\\Users\\Danwei Yao\\Desktop\\poulry_sample.xlsx")

swine=swine[complete.cases(swine),]
as.Date(swine$`Receive date`)
swine$Date <- format(as.Date(swine$`Receive date`), "%Y-%m")
swine$`Sample type`=substring(swine$`Sample type`,1,2)
swine$Type<-factor(swine$`Sample type`,levels = c(17,18,15,16,19),labels = c("Swab","Fecal","Feed","Drinking","Sludge"))

p=swine%>%
  ggplot(aes(x=Date,fill=Type,group=Type))+
  geom_bar(width=0.6,position = "stack")+
  scale_fill_nejm(alpha = 0.5)+
  geom_text(stat='count',aes(label=..count..),position=position_stack(vjust = 0.5),size=5)+
  ylab("Number of samples collected")+
  xlab("Sampling Date")+theme_bw()+
  guides(fill=guide_legend(title ="Sample types"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line=element_line("black"),
        text = element_text(size=18))+
  scale_x_discrete(guide = guide_axis(angle = 45))
#ggtitle( "Swine Farm Sample Collection")

p+plot_annotation(title =  "Poultry Farm Sample Collection",
                  theme=theme(plot.title = element_text(color="#292929",size=22,face="bold",family="Arial Black",hjust = 0.5),
                              plot.caption = element_text(colour = "grey50",face="bold.italic",family = "Arial Black"),
                              plot.background = element_rect("#EBEBEB"),
                              panel.background = element_rect("EbEBEB")))


ggsave("C:\\Users\\Danwei Yao\\Desktop\\poultry_collect1.png",
       width = 40,height = 25,units = "cm",dpi=500,type="cairo-png")
#---------------------------------------------------------------------------------------------------------------------------
# Fig 2--------------------------------------------------------------------------------------------------------------------

patterns=c("^P.*A$","^P.*D$","^P.*U$")
iso<-read.csv("C:\\Users\\Danwei Yao\\Desktop\\poultry_isolates.csv",sep=",",header = T)

swine_isolates=iso%>%
  filter(grepl(paste(patterns,collapse = "|"),i_pid))


swine_isolates$fram=substring(swine_isolates$i_pid,1,2)
swine_isolates$fram<-factor(swine_isolates$fram,levels = c("PD","PE","PF"),labels = c("D","E","F"))
cal_isolandsample<-function(x){
  result=x%>%group_by(fram,i_identified_bacteria_final)%>%
    summarise(n_isolate=n(),number_of_sample=n_distinct(i_pid))
  return(result)
}


result_list<-list()

make_summarytable=function(x){
  for(i in 1:4){
    order=c("^17","^18","19")
    sample=c("Swab","Fecal","Sludge")
    data=x%>%filter(grepl(order[i],isolate_id))
    result_list[[i]]=cal_isolandsample(data)
    result_list[[i]]=cbind(result_list[[i]],type=sample[i])
    
  }
  return(result_list)
}
list1=make_summarytable(swine_isolates)

sample.bact=do.call(rbind,list1)
sample.bact$i_identified_bacteria_final=factor(sample.bact$i_identified_bacteria_final)

#  
fig2= ggplot(sample.bact,aes(x=i_identified_bacteria_final,y=n_isolate,fill=factor(type)))+
  geom_bar(stat = "identity",width=0.6,position = "dodge")+
  scale_fill_manual(values=c("#BC3C2999","#0072B599","#7676B199"))+
  #geom_text(stat='count',aes(label=..count..),position=position_stack(vjust = 0.5),size=2.5)+
  ylab("Number of isolates")+
  xlab("Bacterial Types")+theme_bw()+
  guides(fill=guide_legend(title ="Sample types"))+
  geom_text(aes(label=n_isolate),position=position_dodge(width =0.6),vjust=-0.4,size=5)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line=element_line("black"),
        axis.text.y = element_text(face="bold", family = "Arial",
                                   size=15),
        
        title = element_text(hjust=0.5,face="bold",vjust=0.5,family = "Arial"),
        text=element_text(family = "Arial",size=20
        ),
        plot.background = element_rect("#EBEBEB"))+
 facet_grid(fram~.)+
  scale_y_continuous(limits = c(0, 200))+
  scale_x_discrete(guide = guide_axis(angle = 45),
                   labels=c("E.faecalis","E.faecium","E. coli",
                            "K.pneumoniae",
                            "Salmonella spp","S.aureus"))
fig2+plot_annotation(title =  "Poultry Farm Isolates Summary",
                     
                     theme=theme(plot.title = element_text(color="#292929",size=22,face="bold",family="Arial Black",hjust = 0.5),
                                 plot.caption = element_text(colour = "grey50",face="italic",family = "Arial Black"),
                                 plot.background = element_rect("#EbEBEB"),
                                 panel.background = element_rect("#EbEBEB"))) 



ggsave("C:\\Users\\Danwei Yao\\Desktop\\poultry_isolates2.png",
       width = 40,height = 25,units = "cm",dpi=500,type="cairo-png")  

#--------------------------------------------------------------------------------------------
#Figure 3 heat map---------------------------------------------------------------------------
ast_319<-read_excel("C:\\Users\\Danwei Yao\\Desktop\\Ast_0707.xlsx")

d_farm=ast_319%>%
  filter(grepl("^PD.*",i_pid)&a_bacteria=="ECOLI")#0
d_farm=data.frame(lapply(d_farm, function(x){gsub("<= |< |> ","",x)}))
e_farm=ast_319%>%
  filter(grepl("^PE.*",i_pid)&a_bacteria=="ECOLI")
e_farm=data.frame(lapply(e_farm, function(x){gsub("<= |< |> ","",x)}))
f_farm=ast_319%>%
  filter(grepl("^PF.*",i_pid)&a_bacteria=="ECOLI")
f_farm=data.frame(lapply(f_farm, function(x){gsub("<= |< |> ","",x)}))
#---------------------------------------------------------------------------------------

cut_off=c(8,8,16,4,3,8,4,1,8,0.25,2,8,1,1,4,4,2)
Mic_classification<-function(farm,stage){
  datcount<-data.frame()
  if(stage=="chick"){
    datcount=farm%>%filter(grepl("^P.C.*",i_pid))%>%
      select(1:2,!contains("result"))
  }else if(stage=="grower"){
    datcount=farm%>%filter(grepl("^P.X.*",i_pid))%>%
      select(1:2,!contains("result"))
    
  }else{
    datcount=farm%>%filter(grepl("^P.Y.*",i_pid))%>%
      select(1:2,!contains("result"))
  }
  datcount[,7:41]=lapply(datcount[,7:41],as.numeric)
  dat=datcount%>%
    select_if(~!is.numeric(.)||sum(.)!=0)
  example=dat[,7:dim(dat)[2]]
  example[,1:dim(example)[2]]=lapply(seq(1,dim(example)[2],by=1), function(x)example[,x]=example[,x]/cut_off[x])
  example<-cbind(id=datcount$a_lab_id,example)
  example<-gather(example,a_coamoxiclav_mic:a_cotrimoxazole_mic,key="colname",value = "value")
  example$colname=sapply(strsplit(example$colname,"_"),function(x)x[2])
  example$colname=factor(example$colname,levels = c("coamoxiclav","ampicillin","Chlora.phenicol",
                                                    "cotrimoxazole","tetracyclines","gentamicin","cefoxitin","ceftazidime",
                                                    "ceftriaxone", "cefepime","ciprofloxacin","tobramycin","imipenem",
                                                    "merope","azithromycin","aztreonam","colistin"))
                         return(example)
}

plot.heatmap<-function(data){
  data%>%
    ggplot(aes(x=colname,y=id))+
    geom_tile(aes(fill=cut(value,breaks=c(0,1,4,8,Inf),labels=c("Susceptible","Intermediate","Resistance","Heteroresistance"))),size=0.2,color="white")+
    scale_fill_manual(values=c("#ddf1da","#abdda4","#fdae61","#d53e4f"),na.value="grey90",name="Resistance level")+
    #scale_fill_gradient2(low = "white", mid="lightyellow",high = "red", midpoint=1)+
    #scale_fill_gradientn(colors = colors, breaks = b, labels = format(b))+
    #sscale_fill_gradientn(name="CPU Utilization", colours=pals,  
    #values=vals,limits=c(0,100), breaks = brk)
    scale_x_discrete(guide = guide_axis(angle = 90))+
    theme_grey(base_size = 20)+
    xlab("Antibiotics")+ylab("Isolate Id")+
    theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                             family="Helvetica"),
          text=element_text(family="Times New Roman"),
          axis.text.y=element_text(size=8,
                                   hjust=1,vjust=0.2,color='grey40'),
          axis.text.x=element_text(size=20))
  
  
}
c1=Mic_classification(d_farm,"chick")
c2=Mic_classification(d_farm,"grower")
c3=Mic_classification(d_farm,"finisher")
f1=plot.heatmap(c1)+ggtitle("Chicks")
f2=plot.heatmap(c1)+ggtitle("Grower")
f3=plot.heatmap(c1)+ggtitle("Finisher")
library(ggpubr)
combine1=ggarrange(f1+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),
                             axis.title.x=element_blank()),
             f2+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),
                      axis.title.x=element_blank()),
             f3,
             ncol=1,nrow=3,
             common.legend = T,legend = "bottom")

ggsave("C:\\Users\\Danwei Yao\\Desktop\\farmD_v1.png",
       width = 30,
       height = 38,
       units = "cm",
       dpi = 500)
#-----------------------------------------------------------------------------------------------
#plot waste
Mic_waste<-function(farm,waste){
  datcount<-data.frame()
  if(waste=="sludge"){
    datcount=farm%>%filter(grepl("^P.*U$",i_pid))%>%
      select(1:2,!contains("result"))
  }else if(waste=="influent"){
    datcount=farm%>%filter(grepl("^S.*L$",i_pid))%>%
      select(1:2,!contains("result"))
    
  }else{
    datcount=farm%>%filter(grepl("^S.*E$",i_pid))%>%
      select(1:2,!contains("result"))
  }
  
  datcount[,7:41]=lapply(datcount[,7:41],as.numeric)
  dat=datcount%>%
    select_if(~!is.numeric(.)||sum(.)!=0)
  example=dat[,7:23]
  example[,1:17]=lapply(seq(1,17,by=1), function(x)example[,x]=example[,x]/cut_off[x])
  example<-cbind(id=datcount$a_lab_id,example)
  example<-gather(example,a_coamoxiclav_mic:a_cotrimoxazole_mic,key="colname",value = "value")
  example$colname=sapply(strsplit(example$colname,"_"),function(x)x[2])
  example$colname=factor(example$colname,levels = c("coamoxiclav","ampicillin","Chlora.phenicol",
                                                    "cotrimoxazole","tetracyclines","gentamicin","cefoxitin","ceftazidime",
                                                    "ceftriaxone", "cefepime","ciprofloxacin","tobramycin","imipenem",
                                                    "merope","azithromycin","aztreonam","colistin"))
  return(example)
}

w1=Mic_waste(f_farm,"sludge")
w2=Mic_waste(f_farm,"influent")
w3=Mic_waste(f_farm,"effluent")
waste1=plot.heatmap(w1)+ggtitle("Sludge")
ggsave("C:\\Users\\Danwei Yao\\Desktop\\farmF_waste1.png",
       width = 25,
       height = 20,
       units = "cm",
       dpi = 500)
