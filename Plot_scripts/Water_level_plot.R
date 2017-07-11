#Script to read the previously generated filtered files and produce 1 table with all the data per locality

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")


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


niveles_25027050 <- read.table("25027050_niveles_diarios.txt",sep ="\t")

for(i in 1:length(niveles_25027050$V4)){
  
  if(is.na(niveles_25027050$V4[i]) == TRUE){
    
    niveles_25027050$V4[i] = 2003
  }
  
  
}


niveles_25027050 <- na.omit(niveles_25027050)

niveles_25027050 <- niveles_25027050[niveles_25027050$V1>1975,]

niveles_25027050$V3 <- as.Date(niveles_25027050$V3, "%Y_%m_%d")

# a sequence of all the years in  2011
dates.2011 <- seq( as.Date("2011-01-01"), as.Date("2011-12-31"), by="+1 day")

# a dataframe of four columns and length(dates.2011) or 365 number of rows
df.2011 <- data.frame(matrix(ncol = 4, nrow = length(dates.2011)))

for(d in 1:length(dates.2011)){
  
  df.2011[d,1] <- 2011
  df.2011[d,2] <- d
  df.2011[d,3] <- toString(dates.2011[d])
  df.2011[d,4] <- 2003
  
}



colnames(df.2011) <- colnames(niveles_25027050)

niveles_25027050 <- rbind(niveles_25027050,df.2011)



niveles_25027050_plot = ggplot(data = niveles_25027050, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=niveles_25027050[niveles_25027050$V4==2003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                              panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 1500,
           label = "Station 25027050 \n Margento \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(0, 2500))

#FIN



# STATION 25027270
# reading files and dealing with NA's

niveles_25027270 <- read.table("25027270_niveles_diarios.txt",sep ="\t")
for(i in 1:length(niveles_25027270$V4)){
  
  if(is.na(niveles_25027270$V4[i]) == TRUE){
    
    niveles_25027270$V4[i] = 2003
  }
  
}

niveles_25027270 <- na.omit(niveles_25027270)

niveles_25027270 <- niveles_25027270[niveles_25027270$V1>1975,]

niveles_25027270$V3 <- as.Date(niveles_25027270$V3, "%Y_%m_%d")



niveles_25027270_plot = ggplot(data = niveles_25027270, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=niveles_25027270[niveles_25027270$V4==2003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                              panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 1500,
           label = "Station 25027270 \n Las Flores \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 2500))



# FIN


# STATION 27037010
# NECHI
# reading files and dealing with NA's



niveles_27037010 <- read.table("27037010_niveles_diarios.txt",sep ="\t")

for(i in 1:length(niveles_27037010$V4)){
  
  if(is.na(niveles_27037010$V4[i]) == TRUE){
    
    niveles_27037010$V4[i] = 2003
  }
  
}

niveles_27037010 <- na.omit(niveles_27037010)

niveles_27037010 <- niveles_27037010[niveles_27037010$V1>1975,]

niveles_27037010$V3 <- as.Date(niveles_27037010$V3, "%Y_%m_%d")



niveles_27037010_plot = ggplot(data = niveles_27037010, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=niveles_27037010[niveles_27037010$V4==2003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                              panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 1500,
           label = "Station 27037010 \n La Esperanza \n Nechi river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 2500))


# FIN



# STATION 25027640
#Tres Cruces Cauca river
# reading files and dealing with NA's




niveles_25027640 <- read.table("25027640_niveles_diarios.txt",sep ="\t")


for(i in 1:length(niveles_25027640$V4)){
  
  if(is.na(niveles_25027640$V4[i]) == TRUE){
    
    niveles_25027640$V4[i] = 2003
  }
  
}

niveles_25027640 <- na.omit(niveles_25027640)

niveles_25027640 <- niveles_25027640[niveles_25027640$V1>1975,]

niveles_25027640$V3 <- as.Date(niveles_25027640$V3, "%Y_%m_%d")


niveles_25027640_plot = ggplot(data = niveles_25027640, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=niveles_25027640[niveles_25027640$V4==2003,],aes(x=V3, y=V4), colour="red", size=1)+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                              panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 1500,
           label = "Station 25027640 \n Tres Cruces \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 2500))


#FIN


#STATION 26247030
#Apavi upper cauca river
# reading files and dealing with NA's

niveles_26247030 <- read.table("26247030_niveles_diarios.txt",sep ="\t")



for(i in 1:length(niveles_26247030$V4)){
  
  if(is.na(niveles_26247030$V4[i]) == TRUE){
    
    niveles_26247030$V4[i] = 2003
  }
  
}

niveles_26247030 <- na.omit(niveles_26247030)

niveles_26247030 <- niveles_26247030[niveles_26247030$V1>1975,]

niveles_26247030$V3 <- as.Date(niveles_26247030$V3, "%Y_%m_%d")


niveles_26247030_plot = ggplot(data = niveles_26247030, aes(x=V3, y=V4))+
  geom_point() + geom_point(data=niveles_26247030[niveles_26247030$V4==2003,],aes(x=V3, y=V4), colour="red", size=1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 1500,
           label = "Station 26247030 \n Apavi \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 2500))


#FIN



multiplot(niveles_26247030_plot,  niveles_25027050_plot,niveles_25027640_plot ,cols=1)

