
library(rtf)

output<-"plot_.rtf"

rtf<-RTF(output,width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
addPng(rtf, "scatterplot.png", width = 7, height = 7)
done(rtf)

