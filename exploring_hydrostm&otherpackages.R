# New plots

# https://cran.rstudio.com/web/packages/hydroTSM/hydroTSM.pdf
library(ggplot2)

library(zoo)
library(hydroTSM)
Sys.setlocale("LC_TIME", "English")
# read.zoo(file, format = "", tz = "", FUN = NULL,
#          regular = FALSE, index.column = 1, aggregate = FALSE, ...)

setwd("E:/Sedimentology/Project FINAL/Rproject/Rdirectory/filtered files/")

#25027270

data.dis.25027270 <- read.table('25027270caudales_diarios.txt')

#remove NA's

data.dis.25027270 <- na.omit(data.dis.25027270)
#remove d first two columns
data.dis.25027270 <- data.dis.25027270[,-1]
data.dis.25027270 <- data.dis.25027270[,-1]

colnames(data.dis.25027270) <- c("Date","Discharge")

#data.dis.25027270$V3 <- as.Date(data.dis.25027270$V3, "%Y_%m_%d")

data <- read.zoo(data.dis.25027270, format = "%Y_%m_%d", header = TRUE,sep = "/t")

hydroplot(data,FUN=mean)

hydroplot(data,FUN=mean,ptype = 'ts',pfreq ="dm",tick.tstep="years", lab.tstep="years",lab.fmt= "%Y")

#convert to montly data by summing all the data from the days
m <- daily2monthly(data, FUN=sum, na.rm=TRUE)

#Boxplot of monthly data
cmonth <- format(time(m), "%B")
cmonth <- factor(cmonth,  levels=c("January", "February", "March","April","May","June","July","August","September","October","November","December"))
boxplot(coredata(m) ~ cmonth, col="lightblue", main="ML/month")










data.months <- format(time(m), "%B")
data.months <- factor(cmonth,  levels=c("January", "February", "March","April","May","June","July","August","September","October","November","December"))
data.years <- format(time(m), "%Y")
data.discharge <-coredata(m)

A <- data.frame(matrix(ncol = 3, nrow = length(data.months)))

A[,1]<-data.months
A[,2]<-data.years
A[,3]<-data.discharge

colnames(A) <- c( "month", "year", "discharge")

niveles_25027050_plot = ggplot(data = A, aes(x=month, y=discharge))+ 
  geom_point(aes(group = year, colour = "gray"))+ geom_line(aes(group=year, colour = "gray"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank())



+
  xlab("Date") +ylab("mean daily water level (cm)") +
  annotate("text", x = as.Date("2002_01_01", "%Y_%m_%d"), y = 800,
           label = "Station 25027050 \n Margento \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(200, 900))





#obtain monthly median
m.median <- monthlyfunction(m, FUN=median, na.rm=TRUE)
m.min <- monthlyfunction(m, FUN=min, na.rm=TRUE)
m.max <- monthlyfunction(m, FUN=max, na.rm=TRUE)
m.quartile <- monthlyfunction(m, FUN=quantile, na.rm=TRUE)

df <- as.data.frame(m.quartile)
df <- cbind(df,row.names(df))


