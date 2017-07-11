#Script for checking and plotting if there are NA's within the precipitation data

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")

Sys.setlocale("LC_TIME", "English")


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



breik = as.Date(c("2001_01_01","2002_01_01","2003_01_01","2004_01_01",
                  "2005_01_01","2006_01_01","2007_01_01","2008_01_01",
                  "2009_01_01","2010_01_01","2011_01_01","2012_01_01",
                  "2013_01_01","2014_01_01","2015_01_01"), "%Y_%m_%d")

xlimites = as.Date(c("1975_01_01","2015_12_31"), "%Y_%m_%d")

#load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(hydroTSM)
library(lubridate)

# START data.25020480
#read rainfall data - 25020480_precipitacion_total_diaria
data.25020480 <- read.table("25020480_precipitacion_total_diaria.txt",sep ="\t")


#Loop to find NA's and replace with a constant value, in this case 5003 was chosen
for(i in 1:length(data.25020480$V4)){
  
  if(is.na(data.25020480$V4[i]) == TRUE){
    
    data.25020480$V4[i] = 222.9876
  }
  
  
}

#erase 
data.25020480 <- na.omit(data.25020480)

data.25020480 <- data.25020480[data.25020480$V1>1975,]

data.25020480$V3 <- as.Date(data.25020480$V3, "%Y_%m_%d")


p_25020480_plot = ggplot(data = data.25020480, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=data.25020480[data.25020480$V4==222.9876,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                              panel.background = element_blank()) +
  xlab("Date") +ylab("Daily Precipitation [mm/s]") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 180,
           label = "Station 25020480 \n Los Pájaros \n Ciénaga de Ayapel", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 240))

# END data.25020480

# START data.25020780
#read rainfall data - 25020780_precipitacion_total_diaria
data.25020780 <- read.table("25020780_precipitacion_total_diaria.txt",sep ="\t")


#Loop to find NA's and replace with a constant value, in this case 5003 was chosen
for(i in 1:length(data.25020780$V4)){
  
  if(is.na(data.25020780$V4[i]) == TRUE){
    
    data.25020780$V4[i] = 222.9876
  }
  
  
}

#erase 
data.25020780 <- na.omit(data.25020780)

data.25020780 <- data.25020780[data.25020780$V1>1975,]

data.25020780$V3 <- as.Date(data.25020780$V3, "%Y_%m_%d")


p_25020780_plot = ggplot(data = data.25020780, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=data.25020780[data.25020780$V4==222.9876,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                          panel.background = element_blank()) +
  xlab("Date") +ylab("Daily Precipitation [mm/s]") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 180,
           label = "Station 25020780 \n Cecilia \n Ciénaga de Ayapel", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 240))

# END data.25020780



# START data.25025150
#read rainfall data - 25025150_precipitacion_total_diaria
data.25025150 <- read.table("25025150_precipitacion_total_diaria.txt",sep ="\t")


#Loop to find NA's and replace with a constant value, in this case 5003 was chosen
for(i in 1:length(data.25025150$V4)){
  
  if(is.na(data.25025150$V4[i]) == TRUE){
    
    data.25025150$V4[i] = 222.9876
  }
  
  
}

#erase 
data.25025150 <- na.omit(data.25025150)

data.25025150 <- data.25025150[data.25025150$V1>1975,]

data.25025150$V3 <- as.Date(data.25025150$V3, "%Y_%m_%d")


p_25025150_plot = ggplot(data = data.25025150, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=data.25025150[data.25025150$V4==222.9876,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                          panel.background = element_blank()) +
  xlab("Date") +ylab("Daily Precipitation [mm/s]") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 180,
           label = "Station 25025150 \n Ayapel \n Ciénaga de Ayapel", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 240))

# END data.25025150


multiplot(p_25025150_plot,p_25020780_plot,p_25020480_plot, cols=1)




#Plot filtered filled files also

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files/filter_and_fill")


# START data.25020480_f
data.25020480_f <- read.table("filled_precipitation_25020480.txt",sep ="\t")

data.25020480_f$V1 <- as.Date(data.25020480_f$V1, "%Y-%m-%d")


p_25020480_plot_f = ggplot(data = data.25020480_f, aes(x=V1, y=V2))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  xlab("Date") +ylab("Daily Precipitation [mm/s]") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 180,
           label = "Station 25020480 \n Los Pájaros \n Ciénaga de Ayapel", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 240))

# END data.25020480_f

# START data.25020780_f
data.25020780_f <- read.table("filled_precipitation_25020780.txt",sep ="\t")

data.25020780_f$V1 <- as.Date(data.25020780_f$V1, "%Y-%m-%d")


p_25020780_plot_f = ggplot(data = data.25020780_f, aes(x=V1, y=V2))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  xlab("Date") +ylab("Daily Precipitation [mm/s]") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 180,
           label = "Station 25020780 \n Cecilia \n Ciénaga de Ayapel", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 240))

# END data.25020780_f

# START data.25025150_f
data.25025150_f <- read.table("filled_precipitation_25025150.txt",sep ="\t")

data.25025150_f$V1 <- as.Date(data.25025150_f$V1, "%Y-%m-%d")


p_25025150_plot_f = ggplot(data = data.25025150_f, aes(x=V1, y=V2))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  xlab("Date") +ylab("Daily Precipitation [mm/s]") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 180,
           label = "Station 25025150 \n Ayapel \n Ciénaga de Ayapel", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 240))

# END data.25025150_f



multiplot(p_25025150_plot_f,p_25020780_plot_f,p_25020480_plot_f, cols=1)
