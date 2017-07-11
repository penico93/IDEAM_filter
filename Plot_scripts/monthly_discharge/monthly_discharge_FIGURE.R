# This script takes the corrected-filled water discharge data of two stations and generates a plot


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
colnames(data.dis.25027270) <- c("Date","Discharge")
data.zoo <- read.zoo(data.dis.25027270, format = "%Y-%m-%d", header = TRUE,sep = "/t")

#exploratory plots of the hydrological data
# hydroplot(data.zoo,FUN=mean)
# 
# # Create plots of daily and month data
# hydroplot(data.zoo,FUN=mean,ptype = 'ts',pfreq ="dm",tick.tstep="years", lab.tstep="years",lab.fmt= "%Y")

#convert to montly data by summing or obtaining the mean all the data from the days
m <- daily2monthly(data.zoo, FUN=mean, na.rm=TRUE, date.fmt= "%Y-%m-%d")

#Boxplot of monthly data
cmonth <- format(time(m), "%B")
cmonth <- factor(cmonth,  levels=c("January", "February", "March","April","May","June","July","August","September","October","November","December"))

#Boxplot of the monthly water discharge data
# boxplot(coredata(m) ~ cmonth, col="lightblue", main="ML/month")


#Create a dataframe of discharge data and organize data to plot
data.months <- format(time(m), "%B")
data.years <- format(time(m), "%Y")
dates <- time(m)
discharge.df <- data.frame(matrix(ncol = 4, nrow = length(data.months)))

discharge.df[,1] <- data.years
discharge.df[,2] <- dates
discharge.df[,3] <- coredata(m)
discharge.df[,4] <- format(time(m), "%m")

colnames(discharge.df) <- c( "year", "date", "discharge",'month')

#obtain monthly median

discharge.df.stat <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(discharge.df.stat) <- c( "year", "date", "discharge",'month')

m.median <- monthlyfunction(m, FUN=median, na.rm=TRUE, date.fmt ="%m")
m.median <- as.data.frame(m.median)
rn <- month(as.yearmon(as.data.frame(rownames(m.median))[,1],format="%b"))
m.median <- m.median[,1]
m.median <- cbind(m.median,rn)
#as.yearmon("Apr", format = "%b")
#month("2017-04-01")
median.stat<- data.frame(matrix(ncol = 4, nrow = 12))
median.stat[,1] <- rep(1,length(12))
median.stat[,2] <- rep(1,length(12))
median.stat[,3] <- m.median[,1]
median.stat[,4] <- m.median[,2]
colnames(median.stat) <- c( "year", "date", "discharge",'month')

discharge.df.stat <- rbind(discharge.df.stat,median.stat)


m.min <- monthlyfunction(m, FUN=min, na.rm=TRUE)
m.min <- as.data.frame(m.min)
rn <- month(as.yearmon(as.data.frame(rownames(m.min))[,1],format="%b"))
m.min <- m.min[,1]
m.min <- cbind(m.min,rn)
min.stat <- data.frame(matrix(ncol = 4, nrow = 12))
min.stat[,1] <- rep(2,length(12))
min.stat[,2] <- rep(2,length(12))
min.stat[,3] <- m.min[,1]
min.stat[,4] <- m.min[,2]
colnames(min.stat) <- c( "year", "date", "discharge",'month')

discharge.df.stat <- rbind(discharge.df.stat,min.stat)




m.max <- monthlyfunction(m, FUN=max, na.rm=TRUE)
m.max <- as.data.frame(m.max)
rn <- month(as.yearmon(as.data.frame(rownames(m.max))[,1],format="%b"))
m.max <- m.max[,1]
m.max <- cbind(m.max,rn)
max.stat <- data.frame(matrix(ncol = 4, nrow = 12))
max.stat[,1] <- rep(3,length(12))
max.stat[,2] <- rep(3,length(12))
max.stat[,3] <- m.max[,1]
max.stat[,4] <- m.max[,2]
colnames(max.stat) <- c( "year", "date", "discharge",'month')

discharge.df.stat <- rbind(discharge.df.stat, max.stat)

discharge.df.stat[,1] <- as.numeric(discharge.df.stat[,1])
discharge.df.stat[,2] <- as.numeric(discharge.df.stat[,2])
discharge.df.stat[,3] <- as.numeric(discharge.df.stat[,3])
discharge.df.stat[,4] <- as.numeric(discharge.df.stat[,4])

discharge.df[,1] <- as.numeric(discharge.df[,1])
discharge.df[,2] <- as.numeric(discharge.df[,2])
discharge.df[,3] <- as.numeric(discharge.df[,3])
discharge.df[,4] <- as.numeric(discharge.df[,4])


discharge.df <- rbind(discharge.df,discharge.df.stat)


p1 <- ggplot(data = discharge.df[discharge.df$year %in% c(1999,2000,2008,2009,2010,2011,2012),] ,aes(x=month,y=discharge, colour = factor(year), group=year)) + geom_line(size=0.8)+
  geom_line(color = "black",data = discharge.df[discharge.df$year %in% c(1,2,3),] ,aes(x=month,y=discharge),linetype="dotted",size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(), axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"), axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Mean monthly water discharge")+
   xlab("")+ ylab("Water discharge ("~"m"^"3"*"/s)")+
  scale_x_discrete(limits = c("Jan", "Feb", "March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
  annotate("text", x = 2, y = 4000,
           label = "Station 25027270 \n Las Flores \n Cauca river", size =2)+
  scale_colour_manual(values =c('gray65','springgreen3','saddlebrown','midnightblue','mediumvioletred','olivedrab','steelblue1'))










#Read one of the water discharge files from station 25027050
data.dis.25027050 <- read.table('filled_caud_25027050.txt')

#remove rows that have NA's
data.dis.25027050 <- na.omit(data.dis.25027050)

#give colnames to the dataframe
colnames(data.dis.25027050) <- c("Date","Discharge")

# create a zoo object
data.zoo <- read.zoo(data.dis.25027050, format = "%Y-%m-%d", header = TRUE,sep = "/t")

#exploratory plots of the hydrological data
#hydroplot(data.zoo,FUN=mean)

# Create plots of daily and month data
#hydroplot(data.zoo,FUN=mean,ptype = 'ts',pfreq ="dm",tick.tstep="years", lab.tstep="years",lab.fmt= "%Y")

#convert to montly data by summing or obtaining the mean all the data from the days
m <- daily2monthly(data.zoo, FUN=mean, na.rm=TRUE, date.fmt= "%Y-%m-%d")

#Boxplot of monthly data
cmonth <- format(time(m), "%B")
cmonth <- factor(cmonth,  levels=c("January", "February", "March","April","May","June","July","August","September","October","November","December"))

#Boxplot of the monthly water discharge data
#boxplot(coredata(m) ~ cmonth, col="lightblue", main="ML/month")


#Create a dataframe of discharge data and organize data to plot
data.months <- format(time(m), "%B")
data.years <- format(time(m), "%Y")
dates <- time(m)
discharge.df <- data.frame(matrix(ncol = 4, nrow = length(data.months)))

discharge.df[,1] <- data.years
discharge.df[,2] <- dates
discharge.df[,3] <- coredata(m)
discharge.df[,4] <- format(time(m), "%m")

colnames(discharge.df) <- c( "year", "date", "discharge",'month')

#obtain monthly median

discharge.df.stat <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(discharge.df.stat) <- c( "year", "date", "discharge",'month')

m.median <- monthlyfunction(m, FUN=median, na.rm=TRUE, date.fmt ="%m")
m.median <- as.data.frame(m.median)
rn <- month(as.yearmon(as.data.frame(rownames(m.median))[,1],format="%b"))
m.median <- m.median[,1]
m.median <- cbind(m.median,rn)
#as.yearmon("Apr", format = "%b")
#month("2017-04-01")
median.stat<- data.frame(matrix(ncol = 4, nrow = 12))
median.stat[,1] <- rep(1,length(12))
median.stat[,2] <- rep(1,length(12))
median.stat[,3] <- m.median[,1]
median.stat[,4] <- m.median[,2]
colnames(median.stat) <- c( "year", "date", "discharge",'month')

discharge.df.stat <- rbind(discharge.df.stat,median.stat)


m.min <- monthlyfunction(m, FUN=min, na.rm=TRUE)
m.min <- as.data.frame(m.min)
rn <- month(as.yearmon(as.data.frame(rownames(m.min))[,1],format="%b"))
m.min <- m.min[,1]
m.min <- cbind(m.min,rn)
min.stat <- data.frame(matrix(ncol = 4, nrow = 12))
min.stat[,1] <- rep(2,length(12))
min.stat[,2] <- rep(2,length(12))
min.stat[,3] <- m.min[,1]
min.stat[,4] <- m.min[,2]
colnames(min.stat) <- c( "year", "date", "discharge",'month')

discharge.df.stat <- rbind(discharge.df.stat,min.stat)



  
m.max <- monthlyfunction(m, FUN=max, na.rm=TRUE)
m.max <- as.data.frame(m.max)
rn <- month(as.yearmon(as.data.frame(rownames(m.max))[,1],format="%b"))
m.max <- m.max[,1]
m.max <- cbind(m.max,rn)
max.stat <- data.frame(matrix(ncol = 4, nrow = 12))
max.stat[,1] <- rep(3,length(12))
max.stat[,2] <- rep(3,length(12))
max.stat[,3] <- m.max[,1]
max.stat[,4] <- m.max[,2]
colnames(max.stat) <- c( "year", "date", "discharge",'month')

discharge.df.stat <- rbind(discharge.df.stat, max.stat)

discharge.df.stat[,1] <- as.numeric(discharge.df.stat[,1])
discharge.df.stat[,2] <- as.numeric(discharge.df.stat[,2])
discharge.df.stat[,3] <- as.numeric(discharge.df.stat[,3])
discharge.df.stat[,4] <- as.numeric(discharge.df.stat[,4])

discharge.df[,1] <- as.numeric(discharge.df[,1])
discharge.df[,2] <- as.numeric(discharge.df[,2])
discharge.df[,3] <- as.numeric(discharge.df[,3])
discharge.df[,4] <- as.numeric(discharge.df[,4])


discharge.df <- rbind(discharge.df,discharge.df.stat)


p2 <- ggplot(data = discharge.df[discharge.df$year %in% c(1999,2000,2008,2009,2010,2011,2012),] ,aes(x=month,y=discharge, colour = factor(year), group=year)) + geom_line(size=0.8)+
    geom_line(color = "black",data = discharge.df[discharge.df$year %in% c(1,2,3),] ,aes(x=month,y=discharge),linetype="dotted",size=0.5)+
  theme(legend.key=element_blank(),legend.position='none',text = element_text(size=8), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(), axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"), axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
    ggtitle("Mean monthly water discharge")+
     xlab("")+ ylab("Water discharge ("~"m"^"3"*"/s)")+
  scale_x_discrete(limits = c("Jan", "Feb", "March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
    annotate("text", x = 2, y = 3000,
           label = "Station 25027050 \n Margento \n Cauca river", size =2)+
  scale_colour_manual(values =c('gray65','springgreen3','saddlebrown','midnightblue','mediumvioletred','olivedrab','steelblue1'))



  
#Ayapel levels obtain by Augusto
data.level <- read.table('ayapel_levels.txt', header = TRUE)
data.level$date <- as.Date(data.level$date, "%d/%m/%Y")



#INSERTAR SCRIPT ACA

#read data from the cienaga de Ayapel levels

data.zoo.AYAPEL <- read.zoo(data.level, format = "%Y-%m-%d", header = TRUE,sep = "/t")

m <- data.zoo.AYAPEL

#Boxplot of monthly data
cmonth <- format(time(m), "%B")
cmonth <- factor(cmonth,  levels=c("January", "February", "March","April","May","June","July","August","September","October","November","December"))

#Boxplot of the monthly water LEVEL data
#boxplot(coredata(m) ~ cmonth, col="lightblue", main="ML/month")


#Create a dataframe of LEVEL data and organize data to plot
data.months <- format(time(m), "%B")
data.years <- format(time(m), "%Y")
dates <- time(m)
LEVEL.df <- data.frame(matrix(ncol = 4, nrow = length(data.months)))

LEVEL.df[,1] <- data.years
LEVEL.df[,2] <- dates
LEVEL.df[,3] <- coredata(m)[,1]
LEVEL.df[,4] <- format(time(m), "%m")

colnames(LEVEL.df) <- c( "year", "date", "level",'month')

#obtain monthly median

LEVEL.df.stat <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(LEVEL.df.stat) <- c( "year", "date", "level",'month')


m<-m[,-2]
m<-m[,-2]

m.LEV.median <- monthlyfunction(m, FUN=median, na.rm=TRUE, date.fmt ="%m")
m.LEV.median <- as.data.frame(m.LEV.median)
rn <- month(as.yearmon(as.data.frame(rownames(m.LEV.median))[,1],format="%b"))
m.LEV.median <- m.LEV.median[,1]
m.LEV.median <- cbind(m.LEV.median,rn)
#as.yearmon("Apr", format = "%b")
#month("2017-04-01")
median.stat<- data.frame(matrix(ncol = 4, nrow = 12))
median.stat[,1] <- rep(1,length(12))
median.stat[,2] <- rep(1,length(12))
median.stat[,3] <- m.LEV.median[,1]
median.stat[,4] <- m.LEV.median[,2]
colnames(median.stat) <- c( "year", "date", "level",'month')

LEVEL.df.stat <- rbind(LEVEL.df.stat,median.stat)


m.LEV.min <- monthlyfunction(m, FUN=min, na.rm=TRUE)
m.LEV.min <- as.data.frame(m.LEV.min)
rn <- month(as.yearmon(as.data.frame(rownames(m.LEV.min))[,1],format="%b"))
m.LEV.min <- m.LEV.min[,1]
m.LEV.min <- cbind(m.LEV.min,rn)
min.stat <- data.frame(matrix(ncol = 4, nrow = 12))
min.stat[,1] <- rep(2,length(12))
min.stat[,2] <- rep(2,length(12))
min.stat[,3] <- m.LEV.min[,1]
min.stat[,4] <- m.LEV.min[,2]
colnames(min.stat) <- c( "year", "date", "level",'month')

LEVEL.df.stat <- rbind(LEVEL.df.stat,min.stat)




m.LEV.max <- monthlyfunction(m, FUN=max, na.rm=TRUE)
m.LEV.max <- as.data.frame(m.LEV.max)
rn <- month(as.yearmon(as.data.frame(rownames(m.LEV.max))[,1],format="%b"))
m.LEV.max <- m.LEV.max[,1]
m.LEV.max <- cbind(m.LEV.max,rn)
max.stat <- data.frame(matrix(ncol = 4, nrow = 12))
max.stat[,1] <- rep(3,length(12))
max.stat[,2] <- rep(3,length(12))
max.stat[,3] <- m.LEV.max[,1]
max.stat[,4] <- m.LEV.max[,2]
colnames(max.stat) <- c( "year", "date", "level",'month')

LEVEL.df.stat <- rbind(LEVEL.df.stat, max.stat)

LEVEL.df.stat[,1] <- as.numeric(LEVEL.df.stat[,1])
LEVEL.df.stat[,2] <- as.numeric(LEVEL.df.stat[,2])
LEVEL.df.stat[,3] <- as.numeric(LEVEL.df.stat[,3])
LEVEL.df.stat[,4] <- as.numeric(LEVEL.df.stat[,4])

LEVEL.df[,1] <- as.numeric(LEVEL.df[,1])
LEVEL.df[,2] <- as.numeric(LEVEL.df[,2])
LEVEL.df[,3] <- as.numeric(LEVEL.df[,3])
LEVEL.df[,4] <- as.numeric(LEVEL.df[,4])


LEVEL.df <- rbind(LEVEL.df,LEVEL.df.stat)
#end of recent script



p3 <- ggplot(data = LEVEL.df[LEVEL.df $year %in% c(1999,2000,2008,2009,2010,2011,2012),] ,aes(x=month,y=level, colour = factor(year), group=year)) + geom_line(size=0.8)+
  geom_line(color = "black",data = LEVEL.df[LEVEL.df $year %in% c(1,2,3),] ,aes(x=month,y=level),linetype="dotted",size=1)+
  theme(legend.key=element_blank(),legend.position=c(.9,.5),text = element_text(size=8), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),panel.border = element_blank(), axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"), axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),axis.text.x = element_text(size=9,angle = 0, hjust = 0.5,face = "bold"))+
  ggtitle("Mean monthly wetland water Level")+
   xlab("")+ ylab("Ayapel Wetland level (cms)")+
  scale_x_discrete(limits = c("Jan", "Feb", "March","April","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
  scale_colour_manual(values =c('gray65','gray65','gray65','gray65','mediumvioletred','orange','red'))

  #legend.position=c(.8,.15)
  
  
pdf("mean_monthly_cauca.pdf", width=8.27, height=9)
multiplot(p1,p2,p3,cols = 1)
dev.off()



