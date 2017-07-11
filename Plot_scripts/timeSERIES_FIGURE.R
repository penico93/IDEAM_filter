#TIME SERIES FIGURES
#figure with timeseries of water discharge data (choose here one station), area of the Ayapel cienaga and level of the ayapel cienaga + SOI

# This script takes the corrected-filled water discharge data of two stations and generates a plot
#start and end

start_int = as.Date( c("1991-05-01", "1994-09-01", "1997-04-01", "2002-05-01","2004-06-01","2006-07-01" , "2009-06-01","1995-07-01" ,"1998-06-01","2007-07-01","2010-06-01","2011-07-01") , "%Y-%m-%d")   
end_int= as.Date( c("1992-06-01", "1995-02-01", "1998-04-01", "2003-01-01","2005-03-01","2006-12-01" , "2010-03-01","1996-02-01" ,"2000-01-01","2008-05-01","2011-03-01","2012-01-01"), "%Y-%m-%d") 
index =  c("gray86","gray86","gray86","gray86","gray86","gray86","gray86","slategray1","slategray1","slategray1","slategray1","slategray1")


rects <- data.frame(start=start_int, end=end_int, group=index)



library(ggplot2)
library(zoo)
library(hydroTSM)
library(lubridate)
library(grid)
library(gridExtra)



multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

Sys.setlocale("LC_TIME", "English")


#Set working directory where the filtered and binded files are
setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files/filter_and_fill")


#Read one of the water discharge files from station 25027270
data.dis.25027270 <- read.table('filled_caud_25027270.txt')
data.dis.25027270 <- na.omit(data.dis.25027270)
colnames(data.dis.25027270) <- c("date","discharge")
data.zoo.25027270 <- read.zoo(data.dis.25027270, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25027270 <- daily2monthly(data.zoo.25027270, FUN=mean, na.rm=TRUE, date.fmt= "%Y-%m-%d")
dates.25027270 <- time(m.25027270)
discharge.df.25027270 <- data.frame(matrix(ncol = 2, nrow = length(time(m.25027270))))
discharge.df.25027270[,1] <- dates.25027270
discharge.df.25027270[,2] <- coredata(m.25027270)
colnames(discharge.df.25027270) <- c("date", "discharge")


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.dis.25027270 <- ggplot(data = discharge.df.25027270, aes(x=date,y=discharge)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
        ggtitle("Mean monthly water discharge \n Cauca river")+
        xlab("")+ ylab("Water discharge ("~"m"^"3"*"/s)")+
        annotate("text", x = as.Date("1985","%Y"), y = 4500,label =
                   "Station 25027270 \n Las Flores \n Cauca river", size =2)+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start_int, xmax=end_int, ymin=-Inf,
                                               ymax=Inf, group=group), color="transparent", fill=as.factor(rects$group), alpha=0.3)
#END



#Read one of the water discharge files from station 25027050
data.dis.25027050 <- read.table('filled_caud_25027050.txt')
data.dis.25027050 <- na.omit(data.dis.25027050)
colnames(data.dis.25027050) <- c("date","discharge")
data.zoo.25027050 <- read.zoo(data.dis.25027050, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25027050 <- daily2monthly(data.zoo.25027050, FUN=mean, na.rm=TRUE, date.fmt= "%Y-%m-%d")
dates.25027050 <- time(m.25027050)
discharge.df.25027050 <- data.frame(matrix(ncol = 2, nrow = length(time(m.25027050))))
discharge.df.25027050[,1] <- dates.25027050
discharge.df.25027050[,2] <- coredata(m.25027050)
colnames(discharge.df.25027050) <- c("date", "discharge")


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.dis.25027050 <- ggplot(data = discharge.df.25027050, aes(x=date,y=discharge)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Mean monthly water discharge \n Cauca river")+
  xlab("")+ ylab("Water discharge ("~"m"^"3"*"/s)")+
  annotate("text", x = as.Date("1985","%Y"), y = 3000,label =
             "Station 25027050 \n Margento \n Cauca river", size =2)+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start_int, xmax=end_int, ymin=-Inf,
                                               ymax=Inf, group=group), color="transparent", fill=as.factor(rects$group), alpha=0.3)
#END


#END


#Begin

#Read one of the sediment transport files from station 25027050
data.sedtrans.25027050 <- read.table('filled_sedTRANS_25027050.txt')
data.sedtrans.25027050 <- na.omit(data.sedtrans.25027050)
colnames(data.sedtrans.25027050) <- c("date","discharge")
sedtrans.zoo.25027050 <- read.zoo(data.sedtrans.25027050, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25027050 <- daily2monthly(sedtrans.zoo.25027050, FUN=mean, na.rm=TRUE, date.fmt= "%Y-%m-%d")
dates.25027050 <- time(m.25027050)
sedtrans.df.25027050 <- data.frame(matrix(ncol = 2, nrow = length(time(m.25027050))))
sedtrans.df.25027050[,1] <- dates.25027050
sedtrans.df.25027050[,2] <- coredata(m.25027050)
colnames(sedtrans.df.25027050) <- c("date", "discharge")


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.sedtrans.25027050 <- ggplot(data = sedtrans.df.25027050, aes(x=date,y=discharge)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Mean monthly sediment transport \n Cauca river")+
  xlab("")+ ylab("Sediment transport [Kton/day]")+
  annotate("text", x = as.Date("1985","%Y"), y = 700,label =
             "Station 25027050 \n Margento \n Cauca river", size =2)+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  



#END sed transport MARGENTO

#BEGIN WATER LEVEL
#Read one of the water levle of the cienaga de Ayapel files from station AyapelLevels

data.dis.AyapelLevels <- read.table('ayapel_levels.txt',header = TRUE)
data.dis.AyapelLevels <- na.omit(data.dis.AyapelLevels)
colnames(data.dis.AyapelLevels) <- c("date","levels")
data.dis.AyapelLevels <- data.dis.AyapelLevels[,-3]
data.dis.AyapelLevels <- data.dis.AyapelLevels[,-3]

x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.dis.AyapelLevels <- ggplot(data = data.dis.AyapelLevels, aes(x=as.Date(date,"%d/%m/%Y"),y=levels)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Mean monthly water levels \n Ayapel wetland")+
  xlab("")+ ylab("Water levels [m]")+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  

#END


#read MEI index the files in the xls
library(gdata)


#start and end

start_int = as.Date( c("1991-05-01", "1994-09-01", "1997-04-01", "2002-05-01","2004-06-01","2006-07-01" , "2009-06-01","1995-07-01" ,"1998-06-01","2007-07-01","2010-06-01","2011-07-01") , "%Y-%m-%d")   
end_int= as.Date( c("1992-06-01", "1995-02-01", "1998-04-01", "2003-01-01","2005-03-01","2006-12-01" , "2010-03-01","1996-02-01" ,"2000-01-01","2008-05-01","2011-03-01","2012-01-01"), "%Y-%m-%d") 
index =  c("gray86","gray86","gray86","gray86","gray86","gray86","gray86","slategray1","slategray1","slategray1","slategray1","slategray1")


rects <- data.frame(start=start, end=end, group=index)


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
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +
  geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start_int, xmax=end_int, ymin=-Inf,
                                               ymax=Inf, group=group), color="transparent", fill=as.factor(rects$group), alpha=0.3)



#END

#QBO_Data(1990-2015)
#Read climatic indexes sent by Natalia, make dataframes in the correct order

#read QBO index the files in the xls
library(gdata)

setwd('E:/Sedimentology/R hydrology/Rdirectory/Data')
QBO.df <- read.xls('QBO_Data(1990-2015).xls')
temp.df.QBO <- data.frame(matrix(ncol = 2, nrow = 12*length(QBO.df$year)))
colnames(temp.df.QBO) <- c('date','index_value')
contador = 0
for(i in 1: length(QBO.df$year)){
  for(j in 1:12){
    contador =  contador + 1
    date <- paste( c(QBO.df$year[i],"-",toString(j),"-","01"),collapse="")
    temp.df.QBO[contador,1] <- date
    temp.df.QBO[contador,2] <- QBO.df[i,j]
  }
}


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.dis.QBO_index <- ggplot(data = temp.df.QBO, aes(x=as.Date(date,"%Y-%m-%d"),y=index_value)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Quasi-Biennial Oscillation (QBO)")+
  xlab("")+ ylab("index value")+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  

#END

#TNA_Index(1990-2015)
#Read climatic indexes sent by Natalia, make dataframes in the correct order

#read TNA index the files in the xls
library(gdata)

setwd('E:/Sedimentology/R hydrology/Rdirectory/Data')
TNA.df <- read.xls('TNA_Index(1990-2015).xls')
temp.df.TNA <- data.frame(matrix(ncol = 2, nrow = 12*length(TNA.df$year)))
colnames(temp.df.TNA) <- c('date','index_value')
contador = 0
for(i in 1: length(TNA.df$year)){
  for(j in 1:12){
    contador =  contador + 1
    date <- paste( c(TNA.df$year[i],"-",toString(j),"-","01"),collapse="")
    temp.df.TNA[contador,1] <- date
    temp.df.TNA[contador,2] <- TNA.df[i,j]
  }
}


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.dis.TNA_index <- ggplot(data = temp.df.TNA, aes(x=as.Date(date,"%Y-%m-%d"),y=index_value)) + geom_line(size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Tropical Northern Atlantic Index (TNA)")+
  xlab("")+ ylab("index value")+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3)  

#END

#BEGIN 
#Ayapel_area(1990-2015)
#Read Ayapel Area data from lansat processing

setwd("E:/Sedimentology/Landsat_files/")
data.l5 <- read.table('table_landsat5.txt')
data.l7 <- read.table('table_landsat7.txt')
data.l8 <- read.table('table_landsat8.txt')
data.ayapel <- rbind(data.l5,data.l7,data.l8)
data.ayapel$Date <- as.Date(data.ayapel$Date)
data.ayapel<-data.ayapel[,-3]


x_breaks <- seq(as.Date("1976/1/1"), as.Date("2014/1/1"), by="1 year")
x_labels <- c("1976"," ","1978"," ","1980"," ","1982"," ","1984"," ","1986"," ","1988"," ","1990",
              " ","1992"," ","1994"," ","1996"," ","1998"," ","2000"," ","2002"," ","2004"," ",
              "2006", " ", "2008" ," " ,"2010" ," ", "2012", " ", "2014" )

plot.Ayapel_area <- ggplot(data = data.ayapel[data.ayapel$Date< as.Date("2005-01-01","%Y-%m-%d"),], aes(x=Date, y=Area))+
  geom_line() +  geom_line(data = data.ayapel[data.ayapel$Date> as.Date("2005-01-01","%Y-%m-%d"),])+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Ayapel wetland Area")+
  xlab("")+ ylab("Area ["~"m"^"2"*"]")+
  scale_x_date(breaks = x_breaks, labels = x_labels, limits = c(as.Date("1990/1/1"), as.Date("2014/1/1"))) +   geom_vline(xintercept = as.numeric(as.Date(c("2010-08-17","2000-07-04","2011-02-09"),"%Y-%m-%d")), linetype=2, color = "gray",size=0.3) 
#"Area ["~"m"^"2"*"/s]"
#END





#BEGIN

#Set working directory where the filtered and binded files are
setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files/filter_and_fill")
#SAVE time series
vd = c(getwd(),'/',"monthly_data",'/')
directory = paste(vd,collapse = '')
dir.create(directory,showWarnings = FALSE)


name1 <- paste(c("month_sedTRANS_","25027050",".txt"),collapse = '')
name1 <- paste(c(directory,name1),collapse = '')
write.table(sedtrans.df.25027050, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

name1 <- paste(c("month_watdis_","25027050",".txt"),collapse = '')
name1 <- paste(c(directory,name1),collapse = '')
write.table(discharge.df.25027050, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

name1 <- paste(c("month_watdis_","25027270",".txt"),collapse = '')
name1 <- paste(c(directory,name1),collapse = '')
write.table(discharge.df.25027270, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)


plot.dis.AyapelLevels

pdf("timeSERIES_FIGURE2.pdf", width=8.27, height=9)
#plot.25027270
multiplot(plot.dis.25027050,plot.sedtrans.25027050,plot.dis.AyapelLevels,plot.Ayapel_area,cols = 1)
dev.off()