# compute resistance table 
# this scripts help us to build the resistance table we are interested in. We can simply start by subsetting the data we wanted, and use the bac_restable() to gender
# a table about resistance% for every antibiotics from each bacterial. Combine_tab() aggregates the results from each bacterial together.

#-------------------------Data selection-------------------------
#----------------------------------------------------------------
#-----------------------------------------------------------------

# condition selection : input any condition we want 
# for ex: select elders
elder=merge_data%>%
  filter(Age>=65)
# % load ast data
ast=read_excel("AST_0721.xlsx"
)

# baseline/ follow up /
ast$pid=substring(ast$i_pid,1,5)
elder_ast=ast%>%
  filter(pid%in%elder$PID) %>%# select the people %>%
  filter(stri_sub(i_pid,-2,-2)=="1")%>% # choose baseline pr follow up 
dplyr::select(1:5,contains("result"))

#--------------------------------------------------------------------------------------

# basic functions input 
n_resistance.count=function(x){
  result_table=cbind(RES=sum(x=="RES",na.rm = T))#SUS=b)
  return(result_table)
}
num<-vector()
find_number=function(x){
  bac_data=elder_ast%>% # this should be the name of dataset we want 
    filter(a_bacteria==x)
  for (i in 1:2){
    order=c("^92","^93")
    data=bac_data%>%filter(grepl(order[i],a_lab_id))
    num[i]=nrow(data)
  }
  return(num)
}
# resistance%= res/total number of isolates

bac_restable=function(x,y,z,sample){
  table=subset(x,a_bacteria==y)
  res_tab<-data.frame()
  if(sample=="STOOL"){
    data=table%>%filter(grepl("^92",a_lab_id))
    # use the amount/ sum 
    tmp=t(t(apply(data[,-c(1:5)],2,function(x)n_resistance.count(x))))
    tmp<-round(tmp/z[1],2)*100
  }else{
    data=table%>%filter(grepl("^93",a_lab_id))
    tmp=t(t(apply(data[,-c(1:5)],2,function(x)n_resistance.count(x))))
    tmp<-round(tmp/z[2],2)*100
  }
  
  colnames(tmp)=y
  #datalist=datalist[, colSums(is.na(datalist)) == 0]
  tmp[is.na(tmp)]<- 0
  rownames(tmp)=sapply(strsplit(rownames(tmp),"_"),function(x)x[2])  
  #tmp=tmp[,1:6]
  tmp
  
}



        

# automatically compute the table for 6 bacterials and  arrage the tables together 

# use the combine_table function 
combine_table<-function(data,sample){
tablelist<-list()
for ( i in c(1:6)){
  tablelist[[i]]<-bac_restable(data,bacteria_list[i],number_list[[i]],sample = sample)
}
tablelist


combine_tab=do.call(cbind,tablelist)

combine_tab=as.data.frame(combine_tab)
combine_tab$newcol=rownames(combine_tab)

combine_tab$newcol<-factor(combine_tab$newcol,levels = c("amocla","ampicillin","cefoxitin","ceftazidime","ceftriaxone","cefepime","foxscr","penicillin","oxacillin",
                                                         "tetracyclines",
                                                         "ciprofloxacin","levofloxacin","moxifloxacin",
                                                         "gentamicin","tobramycin","gen500","st1000","vancomycin",
                                                         "azithromycin","erythromycin",
                                                         "merope","imipenem",
                                                         "trisul","chlora","rifampin","clindamycin","nitrofurantoin","tigecycline" ,
                                                         "colistin","aztreonam",
                                                         "daptomycin","linezolid","synerc","dtest1","dtest2")) # set abx level 
combine_tab=combine_tab[order(combine_tab$newcol),]
resistat_tab=combine_tab[,1:6]
resistat_tab
}


# run example 
number_1=find_number("ECOLI")
number_2=find_number("SALSP")
number_3=find_number("SAURS")
number_4=find_number("KPNEU")

number_5=find_number("EFAEL")
number_6=find_number("EFAEM")

bacteria_list=c("ECOLI","SALSP","SAURS","KPNEU","EFAEL","EFAEM")
number_list=list(number_1,number_2,number_3,number_4,number_5,number_6)


t(combine_table(elder_ast,"STOOL"))
t(combine_table(elder_ast,"NASAL"))
