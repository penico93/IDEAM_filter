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

xlimites = as.Date(c("2001_01_01","2015_01_01"), "%Y_%m_%d")


#Niveles 25027050

niveles_25027050 <- read.table("25027050_niveles_diarios.txt",sep ="\t")

niveles_25027050 <- niveles_25027050[-1,]

niveles_25027050 <- niveles_25027050[niveles_25027050$V1>2000,]
niveles_25027050$V3 <- as.Date(niveles_25027050$V3, "%Y_%m_%d")

niveles_25027050_plot = ggplot(data = niveles_25027050, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean daily water level (cm)") +
  annotate("text", x = as.Date("2002_01_01", "%Y_%m_%d"), y = 800,
           label = "Station 25027050 \n Margento \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(200, 900))


#Niveles 25027270

niveles_25027270<- read.table("25027270_niveles_diarios.txt",sep ="\t")

niveles_25027270 <- niveles_25027270[-1,]

niveles_25027270 <- niveles_25027270[niveles_25027270$V1>2000,]
niveles_25027270$V3 <- as.Date(niveles_25027270$V3, "%Y_%m_%d")

niveles_25027270_plot = ggplot(data = niveles_25027270, aes(x=V3, y=V4))+
  geom_point() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       panel.background = element_blank()) +
  xlab("Date") +ylab("mean monthly water level") +
  annotate("text", x = as.Date("2002_01_01", "%Y_%m_%d"), y = 800,
           label = "Station 25027270 \n Las Flores \n Cauca river", size =3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.text.x = element_text(angle=90))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = xlimites) +scale_y_continuous(limits = c(200, 900))


