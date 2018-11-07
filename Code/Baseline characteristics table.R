
##################################################################################
#Baseline characteristics table using R
#Author: Hanmant Patil
#Date: 12th Sep 2018
#################################################################################

#Invoking required Packages
library(gdata) #To rename the variable name
library(haven) #To read the sas dataset
library(reshape2) #To make some changes like transpose
library(plyr) #To subset the dataset
library(rtf) #To create rtf output

#Create project path
Project="D:\\Hanmant\\PhUSE R workshop materials"

#Specify the path as per your local machine

#define function concat
concat=function(folder_path,filename)
{  p=paste(folder_path,filename,sep="\\")
  return(p)}

#Creating raw datasets library
rawdir=concat(Project,"ADaM data")

#Reading raw dataset
adsl1=read_sas(concat(rawdir,"adsl.sas7bdat"), catalog_file = NULL, encoding = NULL)

#subsetting required data by keeping required columns
adsl2=subset(adsl1, select=c( "USUBJID","AGE","TRT01P",
                          "BASEWT","BASEHT","BMI","HIP","WAIST"))

#Transposing the dataset
adsl3 <- melt(adsl2, id=c("USUBJID","TRT01P"))

#Summary of adsl data for each parameter
adsl_sum<- aggregate(subset(adsl3,select=c(value)), by = adsl3[c('TRT01P', 'variable')],
                   FUN=function(x){c(n=length(x),m=mean(x),se=sd(x)/sqrt(length(x)),med=median(x), 
                                     q1=quantile(x, c(0.25), type = 1),q3=quantile(x, c(0.75), type = 1), 
                                     min=min(x), max=max(x))})

#Creating a variables as required
adsl_sum$n1=adsl_sum$value[,1] 
adsl_sum$m2=adsl_sum$value[,2] 
adsl_sum$se3=adsl_sum$value[,3]
adsl_sum$med4=adsl_sum$value[,4]
adsl_sum$q5=adsl_sum$value[,5]
adsl_sum$q6=adsl_sum$value[,6]
adsl_sum$min7=adsl_sum$value[,7]
adsl_sum$max8=adsl_sum$value[,8]

#Subsetting to keep required variables
adsl_sum=subset(adsl_sum,select=-c(value))

#Renaming the variable name
adsl_sum<-rename.vars(adsl_sum,from="variable",to="param")

#Transpose
adsl_sum1 <- melt(adsl_sum,id=c("param","TRT01P"))

#Spliting the dataset
data1<-subset(adsl_sum1,TRT01P=="ACTIVE")
data2<-subset(adsl_sum1,TRT01P=="PLACEBO")

#Rename
data2 <- rename.vars(data2, from = "TRT01P", to = "TRT01P1")
data2 <- rename.vars(data2, from = "value", to = "value1")

#Merging of datasets
adsl_trt=merge(x=data2,y=data1,by=c("param", "variable"),all.y=TRUE,sort=TRUE)

#Sorting the dataset
adsl_trt1=arrange(adsl_trt, param, variable)

#Renaming the param to pass them in rtf
levels(adsl_trt1$param)[levels(adsl_trt1$param)=="AGE"] <- "Age (year)"
levels(adsl_trt1$param)[levels(adsl_trt1$param)=="BASEWT"] <- "Weight (kg)"
levels(adsl_trt1$param)[levels(adsl_trt1$param)=="BMI"] <- "Body Mass Index (kg/m2)"
levels(adsl_trt1$param)[levels(adsl_trt1$param)=="BASEHT"] <- "Height (cm)"
levels(adsl_trt1$param)[levels(adsl_trt1$param)=="HIP"] <- "Hip Circumference (cm)"
levels(adsl_trt1$param)[levels(adsl_trt1$param)=="WAIST"] <- "Waist Circumference (cm)"

levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="n1"] <- "n"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="m2"] <- "Mean"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="se3"] <- "SD"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="med4"] <- "Median"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="q5"] <- "Q1"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="q6"] <- "Q3"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="min7"] <- "Min"
levels(adsl_trt1$variable)[levels(adsl_trt1$variable)=="max8"] <- "Max"

#Rounding of numeric variables as per specification
adsl_trt1$value1=round(adsl_trt1$value1, 1)
adsl_trt1$value=round(adsl_trt1$value, 1)

#Keeping required variables
rtf_out1=subset(adsl_trt1,select=-c( TRT01P1, TRT01P))

#Subsetting to get Max and Q3 in different datasets
rtf_out1_<-subset(rtf_out1,variable=="Max")
rtf_out1_1<-subset(rtf_out1,variable=="Q3")

#Rename
rtf_out1_ <- rename.vars(rtf_out1_, from = "value1", to = "value1_")
rtf_out1_ <- rename.vars(rtf_out1_, from = "value", to = "value_")
levels(rtf_out1_$variable)[levels(rtf_out1_$variable)=="Max"] <- "Min"

rtf_out1_1 <- rename.vars(rtf_out1_1, from = "value1", to = "value1_1")
rtf_out1_1 <- rename.vars(rtf_out1_1, from = "value", to = "value_1")
levels(rtf_out1_1$variable)[levels(rtf_out1_1$variable)=="Q3"] <- "Q1"

#Merging of datasets
rtf_out2=merge(x=rtf_out1,y=rtf_out1_,by=c("param", "variable"),all.x=TRUE,sort=TRUE)
rtf_out3=merge(x=rtf_out2,y=rtf_out1_1,by=c("param", "variable"),all.x=TRUE,sort=TRUE)

levels(rtf_out3$variable)[levels(rtf_out3$variable)=="Min"] <- "Min , Max"
levels(rtf_out3$variable)[levels(rtf_out3$variable)=="Q1"] <- "Q1 , Q3"

rtf_out4<-subset(rtf_out3,variable!="Q3")
rtf_out5<-subset(rtf_out4,variable!="Max")

rtf_out6<-subset(rtf_out5,variable=="Min , Max")
rtf_out7<-subset(rtf_out5,variable=="Q1 , Q3")

rtf_out6$value1 <- paste(rtf_out6$value1,",",rtf_out6$value1_)
rtf_out6$value <- paste(rtf_out6$value,",",rtf_out6$value_)

rtf_out7$value1 <- paste(rtf_out7$value1,",",rtf_out7$value1_1)
rtf_out7$value <- paste(rtf_out7$value,",",rtf_out7$value_1)

rtf_out8<-subset(rtf_out5,variable!="Q1 , Q3")
rtf_out8<-subset(rtf_out8,variable!="Min , Max")

#Appending the datasets
rtf_out9=rbind(rtf_out8, rtf_out7, rtf_out6)
rtf_out10=subset(rtf_out9,select=-c(value1_,value_, value1_1, value_1))

#Sorting the dataset
rtf_out=arrange(rtf_out10, param)

#Adding blank rows after the group of param
df_new <- as.data.frame(lapply(rtf_out, as.character), stringsAsFactors = FALSE)

rtf_out=head(do.call(rbind, by(df_new, rtf_out$param, rbind, "")), -1)

#Replacing duplicate value of parameters
rtf_out$param[duplicated(rtf_out$param)] <- ""

rtf<-RTF("D:\\Hanmant\\Conferences\\PhUSE EU 2018\\Work\\Baseline_char.rtf",width=8.5,height=11,font.size=9,omi=c(1,1.7,1,1))

addHeader(rtf,title="                   Table 14.1.1.1 Baseline Characteristics", 
          subtitle="                               (Safety Analysis Set)",font.size=11,justify=c)

names(rtf_out)<-c("Parameter","    ","Placebo   N=124","Active     N=126")

addTable(rtf,rtf_out[1:21,],font.size=9,row.names=FALSE,NA.string=" ",col.widths=c(2,1,1,1),col.justify=c(rep('L',2),rep('C',2)),header.col.justify=c(rep('L',1),rep('C',3)))
addText(rtf,paste0("N=Number of subjects in the analysis set","\n" ,"SD= Standard deviation, Q1= First quartile, Q3= Third quartile","\n","Min= Minimum, Max= Maximum"), bold=FALSE, italic=FALSE)
 
addPageBreak(rtf, width=8.5,height=11,omi=c(1,1.7,1,1))
addHeader(rtf,title="                   Table 14.1.1.1 Baseline Characteristics", 
          subtitle="                               (Safety Analysis Set)",font.size=11,justify=c)
addTable(rtf,rtf_out[22:42,],font.size=9,row.names=FALSE,NA.string=" ",col.widths=c(2,1,1,1),col.justify=c(rep('L',2),rep('C',2)),header.col.justify=c(rep('L',1),rep('C',3)))
addText(rtf,paste0("N=Number of subjects in the analysis set","\n" ,"SD= Standard deviation, Q1= First quartile, Q3= Third quartile","\n","Min= Minimum, Max= Maximum"), bold=FALSE, italic=FALSE)
done(rtf)

