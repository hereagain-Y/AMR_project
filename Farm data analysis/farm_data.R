library(dplyr)
library(ggplot2)
install.packages("ggpubr")
install.packages("ggstatsplot")
library(ggpubr)
library("ggstatsplot")
# for data -------------------------------
file.choose()
total<-read.csv( "C:\\Users\\Danwei Yao\\Documents\\AMR\\result_0323.csv",sep=",",header=T)
colnames(total)[1]
Swine_total<-total%>%
  filter(grepl("^S.*",ï..i_pid))%>%dplyr::select(1:5,contains("result"))
Poultry_total<-total%>%
  filter(grepl("^P.*",ï..i_pid))%>%dplyr::select(1:5,contains("result"))


#for famer data-----------------------------
a_farm=Swine_total%>%
  filter(grepl("^SA.*",ï..i_pid))
b_farm=Swine_total%>%
  filter(grepl("^SB.*",ï..i_pid))
c_farm=Swine_total%>%
  filter(grepl("^SC.*",ï..i_pid))
#--------------------------------------------
d_farm=Poultry_total%>%
  filter(grepl("^PD.*",ï..i_pid))
e_farm=Poultry_total%>%
  filter(grepl("^PE.*",ï..i_pid))
f_farm=Poultry_total%>%
  filter(grepl("^PF.*",ï..i_pid))






  