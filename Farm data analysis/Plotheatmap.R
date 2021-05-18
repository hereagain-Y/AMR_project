#Heatmap across farms
#*heatmap.data()
#Combining the count data and MIC values together, order them by their corresponding antibiogram length.  
#*Change the antibiotics names into regular 
#*s
#*
#*
#*
#******************************************************************************************************************
#chage  values contains equations into numerics
a_farm=ast_05%>%
  filter(grepl("^SA.*",i_pid)&a_bacteria=="ECOLI")
a_farm=data.frame(lapply(a_farm, function(x){
  gsub("< |= |<|> ","",x)}))

#-------------------------------------------------------------------------------------------------------------------
# Create data that used for heatmap 

heatmap.data<-function(farm,stage){
  mapdata<-data.frame()
  mapdata=right_join(Mic_classification(farm,stage),stagecount.data(farm,stage),by="id")
  mapdata=mapdata[order(mapdata$RES),]
  mapdata$id<- factor(mapdata$id, levels=unique(mapdata$id))
  mapdata$colname=sapply(strsplit(mapdata$colname,"_"),function(x)x[2])
  return(mapdata)
}
t1=heatmap.data(c_farm,"piglet")
t2=heatmap.data(c_farm,"weaner")
t3=heatmap.data(c_farm,"grower")
t4=heatmap.data(c_farm,"finisher")

#---------------------------------

# Plot our heatmap
plot.heatmap<-function(data){
  data%>%
    ggplot(aes(x=colname,y=id))+
    geom_tile(aes(fill=value),colour="white",colour = "white"
    )+scale_fill_distiller(palette = "PuBuGn",limits=c(0,16),na.value = "#de2d26",
                           direction = 1,labels=c("Susceptible","Intermediate","Resistance","8 fold Resistance","16 fold Resistance"))+
    #scale_fill_gradient2(low = "white", mid="lightyellow",high = "red", midpoint=1)+
    #scale_fill_gradientn(colors = colors, breaks = b, labels = format(b))+
    #sscale_fill_gradientn(name="CPU Utilization", colours=pals,  
    #values=vals,limits=c(0,100), breaks = brk)
    scale_x_discrete(guide = guide_axis(angle = 90))+
    xlab("Antibiotics")+ylab("Isolate Id")+
    theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                             family="Helvetica"),
          text=element_text(family="Times New Roman"),
          axis.text.y=element_text(size=5, family="Times New Roman", 
                                   hjust=1))
  
}
#YlGrBu,PuBu,PuBuGn"
p1=plot.heatmap(t1)+ggtitle("Piglet")
p2=plot.heatmap(t2)+ggtitle("Weaner")
p3=plot.heatmap(t2)+ggtitle("Grower")
p4=plot.heatmap(t2)+ggtitle("Finisher")
farm2=ggarrange(p1,p2,p3,p4,ncol=2,nrow = 2,labels = c("a","b","c","d"))
ggsave("~/Desktop/farmC.png",
       width = 30,
       height = 21,
       units = "cm",
       dpi = 500,
       type = "cairo-png")
