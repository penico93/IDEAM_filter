#Script to read the previously generated filtered files and produce 1 table with all the data per locality

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")

Sys.setlocale("LC_TIME", "English")
library(ggplot2)
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

breik = as.Date(c("2001_01_01","2002_01_01","2003_01_01","2004_01_01",
                  "2005_01_01","2006_01_01","2007_01_01","2008_01_01",
                  "2009_01_01","2010_01_01","2011_01_01","2012_01_01",
                  "2013_01_01","2014_01_01","2015_01_01"), "%Y_%m_%d")

xlimites = as.Date(c("1975_01_01","2014_01_01"), "%Y_%m_%d")


#STATION 25027050
# reading files and dealing with NA's


caudales_25027050 <- read.table("25027050caudales_diarios.txt",sep ="\t")

for(i in 1:length(caudales_25027050$V4)){
  
  if(is.na(caudales_25027050$V4[i]) == TRUE){
    
    caudales_25027050$V4[i] = 5003
  }
  
  
}


caudales_25027050 <- na.omit(caudales_25027050)

caudales_25027050 <- caudales_25027050[caudales_25027050$V1>1975,]

caudales_25027050$V3 <- as.Date(caudales_25027050$V3, "%Y_%m_%d")

# a sequence of all the years in  2011
dates.2011 <- seq( as.Date("2011-01-01"), as.Date("2011-12-31"), by="+1 day")

# a dataframe of four columns and length(dates.2011) or 365 number of rows
df.2011 <- data.frame(matrix(ncol = 4, nrow = length(dates.2011)))

for(d in 1:length(dates.2011)){
  
  df.2011[d,1] <- 2011
  df.2011[d,2] <- d
  df.2011[d,3] <- toString(dates.2011[d])
  df.2011[d,4] <- 5003
  
}



colnames(df.2011) <- colnames(caudales_25027050)

caudales_25027050 <- rbind(caudales_25027050,df.2011)

caudales_25027050 <- na.omit(caudales_25027050)

caudales_25027050_plot = ggplot(data = caudales_25027050, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=caudales_25027050[caudales_25027050$V4==5003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027050 \n Margento \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(0, 7500))

#FIN



# STATION 25027270
# reading files and dealing with NA's

caudales_25027270 <- read.table("25027270caudales_diarios.txt",sep ="\t")
for(i in 1:length(caudales_25027270$V4)){
  
  if(is.na(caudales_25027270$V4[i]) == TRUE){
    
    caudales_25027270$V4[i] = 5003
  }
  
}

caudales_25027270 <- na.omit(caudales_25027270)

caudales_25027270 <- caudales_25027270[caudales_25027270$V1>1975,]

caudales_25027270$V3 <- as.Date(caudales_25027270$V3, "%Y_%m_%d")
caudales_25027270 <- na.omit(caudales_25027270)


caudales_25027270_plot = ggplot(data = caudales_25027270, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=caudales_25027270[caudales_25027270$V4==5003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027270 \n Las Flores \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))



# FIN


# STATION 27037010
# NECHI
# reading files and dealing with NA's



caudales_27037010 <- read.table("27037010caudales_diarios.txt",sep ="\t")

for(i in 1:length(caudales_27037010$V4)){
  
  if(is.na(caudales_27037010$V4[i]) == TRUE){
    
    caudales_27037010$V4[i] = 5003
  }
  
}

caudales_27037010 <- na.omit(caudales_27037010)

caudales_27037010 <- caudales_27037010[caudales_27037010$V1>1975,]

caudales_27037010$V3 <- as.Date(caudales_27037010$V3, "%Y_%m_%d")

caudales_27037010 <- na.omit(caudales_27037010)

caudales_27037010_plot = ggplot(data = caudales_27037010, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=caudales_27037010[caudales_27037010$V4==5003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 27037010 \n La Esperanza \n Nechi river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))


# FIN



# STATION 25027640
#Tres Cruces Cauca river
# reading files and dealing with NA's




caudales_25027640 <- read.table("25027640caudales_diarios.txt",sep ="\t")


for(i in 1:length(caudales_25027640$V4)){
  
  if(is.na(caudales_25027640$V4[i]) == TRUE){
    
    caudales_25027640$V4[i] = 5003
  }
  
}

caudales_25027640 <- na.omit(caudales_25027640)

caudales_25027640 <- caudales_25027640[caudales_25027640$V1>1975,]

caudales_25027640$V3 <- as.Date(caudales_25027640$V3, "%Y_%m_%d")

caudales_25027640 <- na.omit(caudales_25027640)

caudales_25027640_plot = ggplot(data = caudales_25027640, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=caudales_25027640[caudales_25027640$V4==5003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027640 \n Tres Cruces \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))


#FIN


#STATION 26247030
#Apavi upper cauca river
# reading files and dealing with NA's

caudales_26247030 <- read.table("26247030caudales_diarios.txt",sep ="\t")



for(i in 1:length(caudales_26247030$V4)){
  
  if(is.na(caudales_26247030$V4[i]) == TRUE){
    
    caudales_26247030$V4[i] = 5003
  }
  
}

caudales_26247030 <- na.omit(caudales_26247030)

caudales_26247030 <- caudales_26247030[caudales_26247030$V1>1975,]

caudales_26247030$V3 <- as.Date(caudales_26247030$V3, "%Y_%m_%d")

caudales_26247030 <- na.omit(caudales_26247030)

caudales_26247030_plot = ggplot(data = caudales_26247030, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=caudales_26247030[caudales_26247030$V4==5003,],aes(x=V3, y=V4), colour="red", size=1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 26247030 \n Apavi \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))


#FIN



#load corrected files
setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files/filter_and_fill")

caudales_26247030_f <- read.table("filled_caud_26247030.txt",sep ="\t")
caudales_26247030_f$V1 <- as.Date(caudales_26247030_f$V1, "%Y-%m-%d")
caudales_25027050_f <- read.table("filled_caud_25027050.txt",sep ="\t")
caudales_25027050_f$V1 <- as.Date(caudales_25027050_f$V1, "%Y-%m-%d")
caudales_25027270_f <- read.table("filled_caud_25027270.txt",sep ="\t")
caudales_25027270_f$V1 <- as.Date(caudales_25027270_f$V1, "%Y-%m-%d")

caudales_26247030_plot_f = ggplot(data = caudales_26247030_f, aes(x=V1, y=V2))+
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 26247030 \n Apavi \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))


caudales_25027270_plot_f = ggplot(data = caudales_25027270_f, aes(x=V1, y=V2))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027270 \n Las Flores \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))



caudales_25027050_plot_f = ggplot(data = caudales_25027050_f, aes(x=V1, y=V2))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027050 \n Margento \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(0, 7500))



multiplot(caudales_26247030_plot,caudales_26247030_plot_f,cols=1)
multiplot(caudales_25027050_plot,caudales_25027050_plot_f,cols=1)
multiplot(caudales_25027270_plot,caudales_25027270_plot_f,cols=1)

multiplot(caudales_25027270_plot_f,caudales_25027050_plot_f,caudales_26247030_plot_f,cols=1)
multiplot(caudales_25027270_plot,caudales_25027050_plot,caudales_26247030_plot,cols=1)



multiplot(caudales_26247030_plot,  caudales_25027050_plot,caudales_25027270_plot,caudales_25027640_plot ,cols=1)


library(hydroTSM)

#read zoo data



dataf.26247030 <- read.zoo(caudales_26247030_f, format = "%Y-%m-%d", header = TRUE,sep = "/t")

hydroplot(dataf.26247030,FUN=mean)

#use fun = sum or fun = mean to obtain monthy mean or sum
hydroplot(dataf.26247030,FUN=sum,ptype = 'ts',pfreq ="dm",tick.tstep="months", lab.tstep="years",lab.fmt= "%Y")



dataf.25027050 <- read.zoo(caudales_25027050_f, format = "%Y-%m-%d", header = TRUE,sep = "/t")

hydroplot(dataf.25027050,FUN=mean)

#use fun = sum or fun = mean to obtain monthy mean or sum
hydroplot(dataf.25027050,FUN=sum,ptype = 'ts',pfreq ="dm",tick.tstep="months", lab.tstep="years",lab.fmt= "%Y")
