#  Sludge pics
Mic_waste<-function(farm,waste){
  datcount<-data.frame()
  if(waste=="sludge"){
    datcount=farm%>%filter(grepl("^S.*U$",i_pid))%>%
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
a_farm$i_pid
a_waste=Mic_waste(a_farm,waste="influent")
b_waste1=Mic_waste(c_farm,waste="sludge")
b_waste2=Mic_waste(c_farm,waste="influent")
b_waste3=Mic_waste(c_farm,waste="effluent")
plot.waste<-function(data){
  data%>%
    ggplot(aes(x=colname,y=id))+
    geom_tile(aes(fill=cut(value,breaks=c(0,0.5,1,4,8,16), labels=c("0-0.5 fold Susceptible","0.5-1 fold Susceptible","Intermediate","Resistance","8-16 fold Resistance"))), colour = "grey")+
    scale_fill_manual(values=rev(brewer.pal(5,"YlGnBu")),na.value="grey90")+
    #scale_fill_distiller(palette = "PuBuGn",limits=c(0,16),na.value = "#de2d26",
                           d#irection = 1,labels=c("Susceptible","Intermediate","Resistance","8 fold Resistance","16 fold Resistance"))+
    #scale_fill_gradient2(low = "white", mid="lightyellow",high = "red", midpoint=1)+
    #scale_fill_gradientn(colors = colors, breaks = b, labels = format(b))+
    #sscale_fill_gradientn(name="CPU Utilization", colours=pals,  
    #values=vals,limits=c(0,100), breaks = brk)
    scale_x_discrete(guide = guide_axis(angle = 90))+
    xlab("Antibiotics")+ylab("Isolate Id")+
    theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                             family="Helvetica",size=15),
          text=element_text(family="Times New Roman"),
          axis.text.y=element_text(size=10, family="Times New Roman", 
                                   hjust=1),
          axis.text.x=element_text(size=15, family="Times New Roman", 
                                   hjust=1))
  
  
}
a1=plot.waste(a_waste)+ggtitle("Influent Water")

w1=plot.waste(b_waste1)+ggtitle("Sludge")
w2=plot.waste(b_waste2)+ggtitle("Influent Water")
w3=plot.waste(b_waste2)+ggtitle("Effluent Water")
farm_waste=ggarrange(w1+theme(axis.text.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.title.x = element_blank()),
               w2+ 
                 theme(axis.text.x = element_blank(),
                       axis.ticks.x = element_blank(),
                       axis.title.x = element_blank()), 
              
               w3,ncol=1,nrow = 3,
               common.legend = T,legend = "right")
farm_c=ggarrange(w1+theme(axis.text.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.x = element_blank()),
                    
                     
                     w3,ncol=1,nrow = 2,
                     common.legend = T,legend = "right")
ggsave("~/Desktop/farma_waste.png",
       width = 20,
       height = 20,
       units = "cm",
       dpi = 500)
