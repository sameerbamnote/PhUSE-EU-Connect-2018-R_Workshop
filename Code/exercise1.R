## Solution 1


#Read the csv file
dat<-read.csv("<>.csv", header = T)

#Initiate the required libraries
library(dplyr)
library(rtf)

#Summarize the data as per specification 
dat2<- plyr::ddply(dat, c("Treatment", "Time"), plyr::summarise, Mean = mean(Response), SD = sd(Response))

#Create RTF output

output<-"tab.rtf"
rtf<-RTF(output, width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
colnames(dat2)<-gsub("\\."," ",colnames(dat2))
addHeader(rtf,title="Summary Statistics", subtitle="<Safety Analysis Set>")
addTable(rtf,as.data.frame(dat2),font.size=10,row.names=FALSE,NA.string="-", col.justify = 'L', 
header.col.justify = 'L')
addText(rtf,"Footnote1:")
addNewLine(rtf)
addText(rtf,"Footnote2:")
done(rtf)


