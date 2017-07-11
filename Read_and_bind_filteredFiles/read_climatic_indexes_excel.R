#Read climatic indexes sent by Natalia, make dataframes in the correct order

#read MEI index the files in the xls
library(gdata)

setwd('E:/Sedimentology/R hydrology/Rdirectory/Data')
MEI.df <- read.xls('MEI_Index(1990-2015).xls')
temp.df.MEI <- data.frame(matrix(ncol = 2, nrow = 12*length(MEI.df$Year)))
colnames(temp.df.MEI) <- c('date','index_value')
contador <- 0
for(i in 1: length(MEI.df$Year)){
  for(j in 1:12){
    contador =  contador + 1
    date <- paste( c(MEI.df$Year[i],"-",toString(j),"-","01"),collapse="")
    temp.df.MEI[contador,1] <- date
    temp.df.MEI[contador,2] <- MEI.df[i,j]
  }
}


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.dis.MEI_index <- ggplot(data = temp.df.MEI, aes(x=as.Date(date,"%Y-%m-%d"),y=index_value)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Multivariate ENSO Index (MEI)")+
  xlab("")+ ylab("index value")+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) 

#END

