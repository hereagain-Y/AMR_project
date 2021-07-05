#' title: select the ids that are considiered having multifrug-resistance 
#'  create a list that includes Ids which are resistant to at least onecategory of Abx 
#' use count fequency to count the duplicated times of each id
#' if indicates 3 , that means this id is resistant to 3 kinds of abx 
# Multidrug resistanc count 

category_list=list(lactams,macrolides,aminoglycosides,quinolones,carbapenems,
                   Tetracycline,rifamycins,sulfonamide,phenicol,lincosamides,nitro,reserve)



mdr_id=sapply(1:8, function(x){
  data=human_ast[,c("i_pid","a_lab_id",category_list[[x]])]
  id=unique(data[apply(data, 1, function(x)any(x=="RES")),]$a_lab_id)
  id
})


mdr_id=unlist(mdr_id)
mdr_id=mdr_id[!is.na(mdr_id)]
count_frquency<-function(variable,thresh,endpoint){
  ind<-ave(rep(1,length(variable)),variable,FUN=length)
  variable[ind>thresh&ind<endpoint]
}

table(ave(rep(1,length(mdr_id)),mdr_id,FUN=length))


No_mdr=unique(count_frquency(mdr_id,0,4))#<=3
Mdr= unique(count_frquency(mdr_id,3,8))#<=3
human_ast$resistance=ifelse(human_ast$a_lab_id%in%No_mdr,"No MDR",
                  ifelse(human_ast$a_lab_id%in%Mdr,"MDR","All SUS"))
