#add bar in every heatmap --
add_bar=function(data){
 plot=ggplot(data,aes(x=colname,y=id))+
   geom_tile(aes(fill=cut(value,breaks=c(0,1,4,8,Inf),labels=c("Susceptible","Intermediate","Resistance","Heteroresistance"))))+
   scale_fill_manual(values=c("#ddf1da","#abdda4","#fdae61","#d53e4f"),na.value="grey90",name="Resistance level")+
   #scale_fill_gradient2(low = "white", mid="lightyellow",high = "red", midpoint=1)+
   #scale_fill_gradientn(colors = colors, breaks = b, labels = format(b))+
   #sscale_fill_gradientn(name="CPU Utilization", colours=pals,  
   #values=vals,limits=c(0,100), breaks = brk)
   scale_x_discrete(guide = guide_axis(angle = 90))+
   theme_grey(base_size = 10)+ #theme_tufte(base_family="Times New Roman")+
   xlab("Antibiotics")+ylab("Isolate Id")+
   theme(title=element_text(hjust=0.5,vjust=0.5, 
                            family="Times New Roman",size=16),
         text=element_text(family="Times New Roman",size=16),
         axis.text.y=element_blank(),
         axis.text.x=element_text(size=18),
         plot.title=element_text(hjust=0.5),
         axis.ticks=element_blank(),
         panel.grid.major.x = element_line(color = "grey48", size = 0.05),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
       )
 
        
legs=ggplot(data,aes(y = id, x = 0)) + geom_point(aes(color =PID), shape = 15, size = 3, show.legend = F) + 
  theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"))
plot+annotation_custom(ggplotGrob(legs), 
                    xmin = .2, xmax = .5, 
                    ymin = 0, ymax = length(unique(data$id))+0.5)
}

groud_c=drug_map%>%
  filter(group=="Female(<65)")%>%
  group_by(abx_group)%>%
  do(plots=add_bar(data=.)+ggtitle(unique(.$abx_group)))
grou
plots$plots[[1]]
plots$plots[[2]]
plot_grid(plots$plots[[1]],plots$plots[[2]],nrow = 2)
library(ggpubr
        )
ggarrange(plots$plots[[1]],plots$plots[[2]],nrow = 2,labels = c("A","B"),heights = c(2,1.5),common.legend = T,legend = "bottom")

#-----------------------------------------------------------------------------
dat2=drug_map%>%
  filter(group=="Male(<65)"&abx_group=="Use Abx")
dat1=drug_map%>%
  filter(group=="Male(<65)"&abx_group=="Didn't use")
dat3=drug_map%>%
  filter(group=="Male(65+)"&abx_group=="Didn't use")
dat4=drug_map%>%
  filter(group=="Male(65+)"&abx_group=="Use Abx")
dat5=drug_map%>%
  filter(group=="Female(<65)"&abx_group=="Didn't use")
dat6=drug_map%>%
  filter(group=="Female(<65)"&abx_group=="Use Abx")
dat7=drug_map%>%
  filter(group=="Female(65+)"&abx_group=="Didn't use")
dat8=drug_map%>%
  filter(group=="Female(65+)"&abx_group=="Use Abx")


p1=add_bar(dat1)+ggtitle("Group1:Use Abx")
p2=add_bar(dat2)+ggtitle("Group1:Didn't use")
p3=add_bar(dat3)+ggtitle("Group2:Didn't use")
p4=add_bar(dat4)+ggtitle("Group2:Use Abx")
p5=add_bar(dat5)+ggtitle("Group3:Didn't use")
p6=add_bar(dat6)+ggtitle("Group3:Use Abx")
p7=add_bar(dat7)+ggtitle("Group4:Didn't use")
p8=add_bar(dat8)+ggtitle("Group4:Use Abx")

dev.offf()

ggarrange(p1,p5,p2,p6,p3,p7,p4,p8,ncol=2,nrow=4,common.legend = T,
          legend = "bottom")

ggsave("C:\\Users\\Danwei Yao\\Desktop\\Grid_pids.png",
       width =50,height = 45,units = "cm",dpi=500,type="cairo-png")


