#  Write up three functions that help to build the heatmaps of ast results based on MIC levels #
#*stage_count(farm,stage):
#calculate the antibiolength in each stage data, simply by inputing farm name and stage names.
#*Mic classification(farm, stage):
#*select the stage and find columns included MIC values, devided them by cut-offs we manually set,than return the
#*dataset in  a long format.
#*
#*
#******************************************************************************************************************

######################Stage Count####################################################################
stagecount.data<-function(farm,stage){
  
  if(stage=="piglet"){
    dat_count=farm%>%filter(grepl("^S.P.*",i_pid))%>%
      select(1:2,contains("result"))
    pigleta=antibiolength.count(dat_count,stage)
    return(pigleta)
  }else if(stage=="weaner"){
    dat_count=farm%>%filter(grepl("^S.W.*",i_pid))%>%
      select(1:2,contains("result"))
    weana=antibiolength.count(dat_count,stage)
    return(weana)
  }else if(stage=="grower"){
    dat_count=farm%>%filter(grepl("^S.G.*",i_pid))%>%
      select(1:2,contains("result"))
    growa=antibiolength.count(dat_count,stage)
    return(growa)
  }else{
    
    dat_count=farm%>%filter(grepl("^S.F.*",i_pid))%>%
      select(1:2,contains("result"))
    finia=antibiolength.count(dat_count,stage)
    return(finia)
  }
  
}
####################MIC Classification###############################################################
cut_off=c(8,8,16,4,3,8,4,1,8,0.25,2,8,1,1,4,4,2)
Mic_classification<-function(farm,stage){
  datcount<-data.frame()
  if(stage=="piglet"){
    datcount=farm%>%filter(grepl("^S.P.*",i_pid))%>%
      select(1:2,!contains("result"))
    }else if(stage=="weaner"){
    datcount=farm%>%filter(grepl("^S.W.*",i_pid))%>%
      select(1:2,!contains("result"))
    
       }else if(stage=="grower"){
         datcount=farm%>%filter(grepl("^S.G.*",i_pid))%>%
          select(1:2,!contains("result"))
        }else{
          datcount=farm%>%filter(grepl("^S.F.*",i_pid))%>%
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
                                                    "merope","azithromycin","aztreonam","colistin")
  return(example)
}
