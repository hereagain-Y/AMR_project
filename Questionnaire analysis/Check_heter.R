#------------------------------------------------------------------
human_mic=ast%>%
  filter(grepl("^C.*S$",i_pid)&a_bacteria=="ECOLI")%>%
  select(1:5, !contains("result"))


cut_off=c(8,8,16,4,3,8,4,1,8,0.25,2,8,1,1,4,4,2)


human_mic=data.frame(lapply(human_mic, function(x){gsub("<= |< |> ","",x)}))


#-----------------------------------------------------
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
                                                      "merope","azithromycin","aztreonam","colistin"))# set column levels
  return(example2)
}


parti_mic=human.Mic(human_mic)
length(unique(parti_mic$sample))#-------------160
#-------requirement 1: each pID with high resistance (>8fold)-----------------------------------
wide_mic=spread(parti_mic,"colname",value)

wide_mic=wide_mic[apply(wide_mic[,3:17], 1, function(x)any(x>8)),]

length(unique(wide_mic$sample))

# requirement2: each PID is in the heteriD list  -----------------------------
heterIDs=substring(heterIDs,1,5)

heter_MIC=wide_mic[wide_mic$sample%in%heterIDs,]

length(unique(wide_mic$sample)) #N=
final_heter_ID=unique(heter_MIC$sample)#N=

total_id=substring(unique(humanstool$i_pid),1,5)

# Final Homo: Intinial Homo + Not_final heter
final_homo_id=unique(total_id[!total_id%in%final_heter_ID])#N=64




intersect(final_homo_id,final_heter_ID) # supoose to be zero

#--------------II Classification---------------------------------------------------------

#A. group by ABX classifications ---------------------------
colnames(humanstool)[6:40]=sapply(strsplit(colnames(humanstool)[6:40],"_"),function(x)x[2])
humanstool$i_pid=substring(humanstool$i_pid,1,5)
lactom_list=humanstool%>%
  filter(i_pid%in%final_heter_ID)%>%
  select(1:2,all_of(lactams))
length(unique(lactom_list$i_pid))
macro_list=humanstool%>%
  filter(i_pid%in%final_heter_ID)%>%
  select(1:2,all_of(macrolides))
  
  
  
  amino_list=humanstool%>%
    filter(i_pid%in%final_heter_ID)%>%
    select(1:2,all_of(aminoglycosides))

  
  
  qunio_list=humanstool%>%
    filter(i_pid%in%final_heter_ID)%>%
    select(1:2,all_of(quinolones))
  
  
  carbe_list=humanstool%>%
    filter(i_pid%in%final_heter_ID)%>%
    select(1:2,all_of(carbapenems))
  
  
  tetra_list=humanstool%>%
    filter(i_pid%in%final_heter_ID)%>%
    select(1:2,all_of(Tetracycline))
  
  unconven_list=humanstool%>%
    filter(i_pid%in%final_heter_ID)%>%
    select(1:2,all_of(uncoventional))
  
  reserve_list=humanstool%>%
    filter(i_pid%in%final_heter_ID)%>%
    select(1:2,all_of(reserve))
  
  
  
  #-------------------Add a column in every antibiotic class inferring whether ID is homo-resistant or heter -------------------------------------------
  nms=names(humanstool)
  class1=lactom_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(amocla))==1&length(unique(ampicillin))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
  
  
  class2=tetra_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(tetracyclines))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
  
  
  class3=qunio_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(ciprofloxacin))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
  
  
  class4=amino_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(gentamicin))==1&length(unique(tobramycin))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)

  
  class5=macro_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(azithromycin))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
  
  class6=reserve_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(colistin))==1&length(unique(aztreonam))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
  
  
  class7=unconven_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(trisul))==1&length(unique(chlora))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
  
  class8=carbe_list%>%
    group_by(i_pid)%>%
    mutate(check=ifelse(length(unique(merope))==1&length(unique(imipenem))==1,"same","diff"))%>%
    select(i_pid,check)%>%
    distinct(i_pid,.keep_all = T)
 #--------Find the "diff" (hetero-resistant) I_pID in every antibiotic class and count the frequency----------------------------
  diff_id=c(class1[class1$check=="diff",]$i_pid,
            class2[class2$check=="diff",]$i_pid,
            class3[class3$check=="diff",]$i_pid,
            class4[class4$check=="diff",]$i_pid,
            class5[class5$check=="diff",]$i_pid,
  class6[class6$check=="diff",]$i_pid,
class7[class7$check=="diff",]$i_pid,
class8[class8$check=="diff",]$i_pid)
  unique(diff_id)#95
  
  #calculate the frequency of each ID
  count_frquency<-function(variable,thresh,endpoint){
    ind<-ave(rep(1,length(variable)),variable,FUN=length)
    variable[ind>thresh&ind<endpoint]
  }

  level2=unique(count_frquency(diff_id,1,3))# hetero-resistance at level 2 32
  level1=unique(count_frquency(diff_id,0,2))# at level 1 47
  level3=unique(count_frquency(diff_id,2,4))
  level4=unique(count_frquency(diff_id,3,5))
  level5=unique(count_frquency(diff_id,4,6))
  ave(rep(1,length(diff_id)),diff_id,FUN=length)
  
  #same way as above
  library(data.table)
level1=unique(as.data.table(diff_id)[,N:=.N,by=diff_id][N==1]$diff_id)
level2=unique(as.data.table(diff_id)[,N:=.N,by=diff_id][N==2]$diff_id)
level3=unique(as.data.table(diff_id)[,N:=.N,by=diff_id][N==3]$diff_id)
level4=unique(as.data.table(diff_id)[,N:=.N,by=diff_id][N==4]$diff_id)
level5=unique(as.data.table(diff_id)[,N:=.N,by=diff_id][N>=5]$diff_id)





