# find the iDs that are all Homo across all antibiotics;
#Update data  
library(readxl)
library("lamisc")
library(tidyverse)
question=read_excel("~/Desktop/Questionnaire_20210611.xlsx")
ast<-read_excel("~/Desktop/ast0618.xlsx")


humanstool=ast%>%
  filter(grepl("^C.*S$",i_pid)&a_bacteria=="ECOLI")%>%
  select(1:5, contains("result"))
dim(humanstool)
length(unique(humanstool$i_pid))
#------------------------------------------------------------------------------

#--------------write a function to count the duplicates in row-----------------

count_duplicates <- function(data, ...) {
  
  uq_names <- as.list(substitute(list(...)))[-1L]
  df_name <- deparse(substitute(data))
  
  if (length(uq_names) == 0) {
    # if called on an entire data.frame with no specified variable names
    var_names <- names(data)
    nms <- rlang::syms(var_names)
    message("No variable names specified - using all columns.\n")
  } else {
    # nms <- rlang::quos(X2:X3) %>%
    #   rlang::quos_auto_name()
    # var_names <- names(nms)
    nms <- rlang::enquos(...)
  }
  
  df <- data %>%
    dplyr::select(!!! nms)
  
  x <- do.call('paste', c(df, sep = '\r'))
  ox <- order(x)
  rl <- rle(x[ox])
  cbind(df[ox[cumsum(rl$lengths)], , drop = FALSE], dupe_count = rl$lengths)
  
}



length(unique(humanstool$i_pid)) #N=194


dupe_list=humanstool%>%select(i_pid,a_amocla_result:a_vancomycin_result)%>%
  group_by(i_pid)%>%
  mutate(count=n())%>%
  count_duplicates()%>%
  select(i_pid,count,dupe_count)%>%
  mutate(check_resistance=ifelse(dupe_count==count,"homo","heter"))
  



# if only one column---------------------------------------------------------------------------------------------
within(humanstool,  # attach the columns of df in a separate environment using within()
       check <- unlist( by(humanstool,   # group df by ID using by()
                           INDICES = i_pid, # check for unique of days with length = 1, if so return true else false
                           FUN = function(x) rep( length( unique( x$name ) ) == 1, length(x$name) ) ) 
       ))
#----------------------------------------------------------------------------------------------------------------



#
homoIDs=unique(dupe_list[dupe_list$check_resistance=="homo",]$i_pid) #N=

heterIDs=unique(dupe_list[!dupe_list$i_pid%in%homoIDs,]$i_pid)#N=

#select more than 8 fold heter IDs 




