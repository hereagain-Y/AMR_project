# for animal food local and import 
ast_05<-read_xlsx("C:\\Users\\Danwei Yao\\Desktop\\ast_0527.xlsx")
dim(ast_05)
food_local<-ast_05%>%
  filter(grepl("^A.*L$",i_pid)&a_bacteria=="ECOLI")%>%
  select(i_pid,a_lab_id,!contains("result"))

food_local=data.frame(lapply(food_local, function(x){
  gsub("< |= |<|> ","",x)}))


food_import<-ast_05%>%
  filter(grepl("^A.*I$",i_pid)&a_bacteria=="ECOLI")%>%
  select(i_pid,a_lab_id,!contains("result"))

food_import=data.frame(lapply(food_import, function(x){
  gsub("< |= |<|> ","",x)}))

veg_unknow<-ast_05%>%
  filter(grepl("^V.*U$",i_pid)&a_bacteria=="ECOLI")%>%
  select(i_pid,a_lab_id,!contains("result"))

veg_unknow=data.frame(lapply(veg_unknow, function(x){
  gsub("< |= |<|> ","",x)}))

cut_off=c(8,8,16,4,3,8,4,1,8,0.25,2,8,1,1,4,4,2)
human.Mic<-function(humandata){
  datcount<-humandata
  datcount[,7:41]=lapply(datcount[,7:41],as.numeric)
  dat=datcount%>%
    select_if(~!is.numeric(.)||sum(.)!=0)
  example2=dat[,7:dim(dat)[2]]
  example2[,1:dim(example2)[2]]=lapply(seq(1,dim(example2)[2],by=1), function(x)example2[,x]=example2[,x]/cut_off[x])
  example2<-cbind(sample=datcount$i_pid,id=datcount$a_lab_id,example2)
  example2<-gather(example2,a_coamoxiclav_mic:a_cotrimoxazole_mic,key="colname",value = "value")
  example2$colname=sapply(strsplit(example2$colname,"_"),function(x)x[2])
  example2$colname=factor(example2$colname,levels = c("coamoxiclav","ampicillin","Chlora.phenicol",
                                                      "cotrimoxazole","tetracyclines","gentamicin","cefoxitin","ceftazidime",
                                                      "ceftriaxone", "cefepime","ciprofloxacin","tobramycin","imipenem",
                                                      "merope","azithromycin","aztreonam","colistin"))
  return(example2)
}

food_mapd=human.Mic(food_local)
foodim_mapd=human.Mic(food_import)
veg_u=human.Mic(veg_unknow)

range(food_mapd$value)

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
    theme_grey(base_size = 10)+
    xlab("Antibiotics")+ylab("Isolate Id")+
    theme(title=element_text(hjust=0.5, face="bold", vjust=0.5, 
                             family="Helvetica"),
          text=element_text(family="Times New Roman"),
          axis.text.y=element_text(size=6,
                                   hjust=1,vjust=0.2,color='grey40'),
          axis.text.x=element_text(size=12))
  
  
}
food_plot=plot.heatmap(food_mapd)+ggtitle("Local Meat")
food_plot2=plot.heatmap(foodim_mapd)+ggtitle("Import Meat")
ve_plot=plot.heatmap(veg_u)+ggtitle("Vegetable")
c1=ggarrange(food_plot+theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),
                   axis.title.x=element_blank()),
          food_plot2,
        ncol=1,nrow=2,
          common.legend = T,legend = "bottom",heights = c(2,1))




ggsave(c1,filename = 'C:\\Users\\Danwei Yao\\Desktop\\food.png',height = 12,
       width = 10,units = "in",dpi=300)


leg <- food_mapd%>%
  ggplot(aes(y = id, x = 0)) + geom_point(aes(color = sample), shape = 15,alpha=0.8, size = 3, show.legend = F) + 
  theme_classic() + 
  theme(axis.title = element_blank(), axis.line = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(), 
        plot.margin = unit(c(0,0,0,0), "cm"))
p+annotation_custom(ggplotGrob(leg), 
                    xmin = .2, xmax = .5, 
                    ymin = 0, ymax = length(unique(data$id))+0.5)
#----------------------------------------------------
#poultry
poultry<-ast_05%>%
  filter(grepl("^P.*",i_pid)&a_bacteria=="ECOLI")%>%
  select(i_pid,a_lab_id,!contains("result"))
poultry=data.frame(lapply(poultry, function(x){
  gsub("< |= |<|> ","",x)}))

swine<-ast_05%>%
  filter(grepl("^S.*",i_pid)&a_bacteria=="ECOLI")%>%
  select(i_pid,a_lab_id,!contains("result"))
swine=data.frame(lapply(swine, function(x){
  gsub("< |= |<|> ","",x)}))
dim(swine)


env<-ast_05%>%
  filter(grepl("^E.*",i_pid)&a_bacteria=="ECOLI")%>%
  select(i_pid,a_lab_id,!contains("result"))
env=data.frame(lapply(env, function(x){
  gsub("< |= |<|> ","",x)}))

env_r=env%>%
  filter(grepl("^E.*R$",i_pid))
env_b=env%>%
  filter(grepl("^E.*B$",i_pid))
pou_map=human.Mic(poultry)
swi_map=human.Mic(swine)
env_map=human.Mic(env)
envr_map=human.Mic(env_r)
envb_map=human.Mic(env_b)

pou_plot=plot.heatmap(pou_map)+ggtitle("Poultry")
swi_plot=plot.heatmap(swi_map)+ggtitle("Swine")
env_plot=plot.heatmap(env_map)+ggtitle("Environment")
river=plot.heatmap(envr_map)+ggtitle("Environment: River")
beach=plot.heatmap(envb_map)+ggtitle("Environment: Beach")
watersample=ggarrange(river,beach,
          ncol=2,nrow=1,
          common.legend = T,legend = "bottom",heights = c(1,1))
ggsave(watersample,filename = 'C:\\Users\\Danwei Yao\\Desktop\\river&beach_env.png',height = 10,
       width = 12,units = "in",dpi=300)

