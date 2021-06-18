#' @title
#' Find monthly missing isolates that forget to do AST test.
#'

#' @references
#'
#' @export
#'
#' @examples \dontrun{
#' # Load some packages
#' library(dplyr)
#' library(tibble)
#' library(ggplot2)
#' library(sqldf)
#' # List of loaded packages -- before
#' (.packages())
#'

library(dplyr)
library(tidyverse)
library(xlsx)
library(sqldf)
file.choose()
#load data for use-------------------------------------------------------------------------------
ast_April<-read.csv("C:\\Users\\Danwei Yao\\Documents\\AMR\\data\\ast_0413.csv",sep=",",header=TRUE)

iso_check=read.csv("C:\\Users\\Danwei Yao\\Documents\\AMR\\data\\checked_isolates.csv",sep=",",header=TRUE)

# substr the ids: using the lab_id for substring
lab_id=ast_April$a_lab_id

#----------------------------------------------------------------------------------
iso_check$isolates_id=substring(iso_check$isolate_id,1,nchar(iso_check$isolate_id)-1)
ast_April$ast_lab_id=substring(ast_April$a_lab_id,1,nchar(ast_April$a_lab_id)-1)
# find the lab id that partly appear both in  ast and iso-checked list
final_table=subset(iso_check,isolates_id%in%ast_lab_id)

# render the differences lab ids comparing with ast 
d1=subset(final_table,isolate_id%in%lab_id)
missingtable=sqldf('select * from final_table except select * from d1')
write.xlsx(missingtable,file="C:\\Users\\Danwei Yao\\Documents\\AMR\\Output\\missing_isolates0413.xlsx",row.names = T,showNA = F)
