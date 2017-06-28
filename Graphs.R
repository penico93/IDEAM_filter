#Script to read the previously generated filtered files and produce 1 table with all the data per locality

setwd("E:/Sedimentology/Project FINAL/Rproject/Rdirectory/filtered files/")

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

xlimites = as.Date(c("1966_01_01","2015_01_01"), "%Y_%m_%d")


#caudales_25027050


caudales_25027050 <- read.table("25027050caudales_diarios.txt",sep ="\t")

caudales_25027050 <- caudales_25027050[-1,]

caudales_25027050 <- caudales_25027050[caudales_25027050$V1>1960,]
caudales_25027050$V3 <- as.Date(caudales_25027050$V3, "%Y_%m_%d")



caudales_25027050_plot = ggplot(data = caudales_25027050, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027050 \n Margento \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(0, 7500))



#caudales_25027270


caudales_25027270 <- read.table("25027270caudales_diarios.txt",sep ="\t")

caudales_25027270 <- caudales_25027270[-1,]

caudales_25027270 <- caudales_25027270[caudales_25027270$V1>1960,]
caudales_25027270$V3 <- as.Date(caudales_25027270$V3, "%Y_%m_%d")



caudales_25027270_plot = ggplot(data = caudales_25027270, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 25027270 \n Las Flores \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))



#
#caudales_27037010 la esperanza nechi


caudales_27037010 <- read.table("27037010caudales_diarios.txt",sep ="\t")

caudales_27037010<- caudales_27037010[-1,]

caudales_27037010 <- caudales_27037010[caudales_27037010$V1>1960,]
caudales_27037010$V3 <- as.Date(caudales_27037010$V3, "%Y_%m_%d")



caudales_27037010_plot = ggplot(data = caudales_27037010, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 27037010 \n La Esperanza \n Nechi river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))






# caudales_25027640 Tres Cruces Cauca river




caudales_25027640 <- read.table("25027640caudales_diarios.txt",sep ="\t")

caudales_25027640<- caudales_25027640[-1,]

caudales_25027640 <- caudales_25027640[caudales_25027640$V1>1960,]
caudales_25027640$V3 <- as.Date(caudales_25027640$V3, "%Y_%m_%d")



caudales_25027640_plot = ggplot(data = caudales_25027640, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 27037010 \n Tres Cruces \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))



#caudales_26247030 Apavi upper cauca river

caudales_26247030 <- read.table("26247030caudales_diarios.txt",sep ="\t")

caudales_26247030<- caudales_26247030[-1,]

caudales_26247030 <- caudales_26247030[caudales_26247030$V1>1960,]
caudales_26247030$V3 <- as.Date(caudales_26247030$V3, "%Y_%m_%d")



caudales_26247030_plot = ggplot(data = caudales_26247030, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2005_01_01", "%Y_%m_%d"), y = 6000,
           label = "Station 26247030 \n Apavi \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) + scale_y_continuous(limits = c(0, 7500))


#diferencia de caudales 25027270 y 25027640

xlimites2 = as.Date(c("2008_01_01","2015_01_01"), "%Y_%m_%d")
dif_caudal = caudales_25027640
dif_caudal$V4  =  caudales_25027640$V4 - caudales_25027270$V4


dif_caudal$V3 = as.Date(dif_caudal$V3, "%Y_%m_%d")

dif_caudal_plot = ggplot(data = dif_caudal, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily discharge ("~"m"^"3"*"/s)") +
  annotate("text", x = as.Date("2011_01_01", "%Y_%m_%d"), y = 2200,
           label = "Difference in water discharge \n 25027640 (lowest) - 25027270 ", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites2) + 
  scale_y_continuous(limits = c(-3000, 3000)) + geom_hline(yintercept = 0)

dif_caudal_plot
#multiplot(caudales_26247030_plot, caudales_25027050_plot,  caudales_25027270_plot,caudales_25027640_plot ,cols=1)

#,caudales_27037010_plot

multiplot(caudales_25027050_plot,  caudales_25027270_plot,caudales_25027640_plot ,cols=1)

                     