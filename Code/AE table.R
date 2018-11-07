
##################################################################################
#Summary of Adverse Events table using R
#Author: Hanmant Patil
#Date: 12th Sep 2018
##################################################################################
#Invoking required Packages
library(gdata)     #To rename the variable name
library(haven)     #To read the sas dataset
library(reshape2)  #To make some changes like transpose
library(plyr)      #To subset the dataset
library(tidyverse) #A set of packages that work in harmony because they share common data representations
library(rowr)      #To add the rows
library(rtf)       #To create rtf output

#Create project path
Project="D:\\Hanmant\\PhUSE R workshop materials"

#define function concat
concat=function(folder_path,filename)
{  p=paste(folder_path,filename,sep="\\")
  return(p)}

#Creating raw datasets library
rawdir=concat(Project,"ADaM data")

#Reading raw dataset
adae1=read_sas(concat(rawdir,"adae.sas7bdat"), catalog_file = NULL, encoding = NULL)

#subsetting required data by keeping required columns
adae2=subset(adae1, select=c( "USUBJID","TRT01P",
                              "AEDECODA","AEBODSYS","AESEV","ANYAEFL", "AESTDT"))

#subsetting to get only AE data
adae3<-subset(adae2,ANYAEFL=="Y")

#Keeping required variables
adae4 <- adae3[order(adae3$USUBJID, adae3$TRT01P,adae3$AEDECODA, adae3$AESTDT), ]

#Keeping distinct data
adae5<-distinct(adae4, USUBJID,TRT01P,AEDECODA)

#Count by treatment and preferred term
adae6<-adae5 %>%
  group_by(TRT01P,AEDECODA) %>%
  tally

#Count by treatment only
adae5_<-distinct(adae4, USUBJID,TRT01P)
adae6_<-adae5_ %>%
  group_by(TRT01P) %>%
  tally

#Keeping required datasets for below part of table
data1<-subset(adae6,TRT01P=="ACTIVE")
data2<-subset(adae6,TRT01P=="PLACEBO")

#Keeping required datasets for upper part of table
data3<-subset(adae6_,TRT01P=="ACTIVE")
data4<-subset(adae6_,TRT01P=="PLACEBO")

#Rename
data2 <- rename.vars(data2, from = "TRT01P", to = "TRT01P1")
data2 <- rename.vars(data2, from = "n", to = "n_plb")

#Upper part of the table
data4 <- rename.vars(data4, from = "n", to = "n_plb")
data4$TRT01P="ACTIVE"

#Merging of datasets
adae7=merge(x=data2,y=data1,by=c("AEDECODA"),all.x=TRUE,all.y=TRUE,sort=TRUE)
adae7_=merge(x=data4,y=data3,by=c("TRT01P"),all.x=TRUE,all.y=TRUE,sort=TRUE)

#Calculation for the percetage
adae7$percnt <- round((adae7$n / 126 * 100),1)
adae7$percnt_plb <- round((adae7$n_plb / 124 * 100),1)

adae7_$percnt <- round((adae7_$n / 126 * 100),1)
adae7_$percnt_plb <- round((adae7_$n_plb / 124 * 100),1)

#concatenating for diplay pupose 
adae7$TRT <- paste(adae7$n,"(",adae7$percnt,")")
adae7$PLB <- paste(adae7$n_plb,"(",adae7$percnt_plb,")")

adae7_$TRT <- paste(adae7_$n,"(",adae7_$percnt,")")
adae7_$PLB <- paste(adae7_$n_plb,"(",adae7_$percnt_plb,")")

#Upper part of the table
adae7_ <- rename.vars(adae7_, from = "TRT01P", to = "AEDECODA")
adae7_$AEDECODA="Number of subjects reporting treatment-emergent adverse events"

#Keeping required variables
rtf_out1=subset(adae7,select=c(AEDECODA,TRT, PLB))
rtf_out1_=subset(adae7_,select=c(AEDECODA,TRT, PLB))

#Replacing NA by "-"
rtf_out2 <- as.data.frame(sapply(rtf_out1,gsub,pattern="NA",replacement="-"))

#Appending both datasets
rtf_out3<-rbind(rtf_out1_, rtf_out2)

#Adding blank rows
rtf_out4<-insertRows(rtf_out3,data.frame(list(' ',' ',' ')),1)
rtf_out<-insertRows(rtf_out4,data.frame(list(' ',' ',' ')),3)

#Reordering the columns
rtf_out = rtf_out %>% select(AEDECODA, PLB, TRT)
#View(rtf_out)

rtf<-RTF("D:\\Hanmant\\Conferences\\PhUSE EU 2018\\Work\\AE_pt.rtf",width=8.5,height=11,font.size=9,omi=c(1,1.25,1,1))

addHeader(rtf,title="           Table 14.1.1.2 Treatment-emergent Adverse Events by Preferred Term", 
          subtitle="                                     (Safety Analysis Set)",font.size=11,justify=c)
names(rtf_out)<-c("Preferred Term", "Placebo   N=124     n(%)","Active     N=126     n(%)")

addTable(rtf,rtf_out[1:46,],font.size=9,row.names=FALSE,NA.string=" ",col.widths=c(4,1,1),col.justify=c(rep('L',1),rep('C',2)),header.col.justify=c(rep('L',1),rep('C',2)))
addText(rtf,paste0("N=Number of subjects in the analysis set","\n","Adverse events are coded using MedDRA version 20.0"), bold=FALSE, italic=FALSE)


addPageBreak(rtf, width=8.5,height=11,omi=c(1,1.25,1,1))
addHeader(rtf,title="           Table 14.1.1.2 Treatment-emergent Adverse Events by Preferred Term", 
          subtitle="                                     (Safety Analysis Set)",font.size=11,justify=c)
addTable(rtf,rtf_out[47:92,],font.size=9,row.names=FALSE,NA.string=" ",col.widths=c(4,1,1),col.justify=c(rep('L',1),rep('C',2)),header.col.justify=c(rep('L',1),rep('C',2)))
addText(rtf,paste0("N=Number of subjects in the analysis set","\n","Adverse events are coded using MedDRA version 20.0"), bold=FALSE, italic=FALSE)

done(rtf)

