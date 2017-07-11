# FILL NA'S IN THE TIME SERIES - water discharge
# Here I will plot the time series and check where they have NA's and compare among stations
# After thaat I will fitler the water discharge data and display it

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")
Sys.setlocale("LC_TIME", "English")

#load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(hydroTSM)

# START data.25020480
#read water discharge from file
data.25020480 <- read.table("25020480_precipitacion_total_diaria.txt",sep ="\t")

data.25020480 <- na.omit(data.25020480)
data.25020480 <- data.25020480[data.25020480$V1>1975,]
data.25020480$V3 <- as.Date(data.25020480$V3, "%Y_%m_%d")
data.25020480 <- data.25020480[,-1]
data.25020480 <- data.25020480[,-1]


zoo.25020480<- read.zoo(data.25020480, format = "%Y-%m-%d", header = TRUE,sep = "/t")

m.25020480 <- daily2monthly(zoo.25020480, FUN=sum, na.rm=TRUE)

m.median.25020480 <- monthlyfunction(m.25020480, FUN=median, na.rm=TRUE)/30


#exploratory plots of the hydrological data
hydroplot(zoo.25020480,FUN=mean)

# Create plots of daily and month data
hydroplot(zoo.25020480,FUN=sum,ptype = 'ts+boxplot',pfreq ="dm",tick.tstep="years", lab.tstep="years",lab.fmt= "%Y")

#Boxplot of monthly data
cmonth <- format(time(m.25020480), "%b")
cmonth <- factor(cmonth,  levels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#Boxplot of the monthly water discharge data
boxplot(coredata(m.25020480) ~ cmonth, col="lightblue", main="Precipitation - Station 25020480",
        xlab = "Month",
        ylab = "Total monthly precipitation [mm]")


#Ayapel area

data.level <- read.table('ayapel_levels.txt', header = TRUE)
data.level$date <- as.Date(data.level$date, "%d/%m/%Y")
data.level <- na.omit(data.level)
data.level <- data.level[,-3]
data.level <- data.level[,-3]
head(data.level)


ayapel.month <- format(data.level$date, "%b")
ayapel.month  <- factor(ayapel.month ,  levels=c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
boxplot(data.level$level ~ ayapel.month, col="lightblue", main="Mean monthly water level - Ayapel wetland",
        xlab = "Month",
        ylab = " water level [cm]")






