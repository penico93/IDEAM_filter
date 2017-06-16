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



#multiplot(ccplot, hmplot, hm_tplot, hrplot, cols=2)


caudales_25027050 <- read.table("25027050caudales_diarios.txt",sep ="\t")

caudales_25027050 <- caudales_25027050[-1,]

caudales_25027050 <- caudales_25027050[caudales_25027050$V1>2000,]
caudales_25027050$V3 <- as.Date(caudales_25027050$V3, "%Y_%m_%d")

caudales_25027050_plot = ggplot(data = caudales_25027050, aes(x=V3, y=V4)) 
caudales_25027050_plot + geom_point()





cc = read.table("CC2.txt", header = TRUE, sep="\t")

ccplot = ggplot(data = cc, aes(x=Clast_Type, y=Percentage)) + geom_boxplot(outlier.colour = NULL)+ theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+ scale_fill_grey() + guides(fill=FALSE)+scale_x_discrete(name = "Clast types",limits=c("Quartzarenite","Lithic arenite","Quartzite","Mudstone","Coquina","Micrite","Mica-schist"))



hr_hm = read.table("highres_hm.txt", header = TRUE)
hrplot = ggplot(data = hr_hm, aes(x=Roundness, y=Percentage)) + geom_boxplot(outlier.colour = NULL)+ theme_bw() + theme(axis.text.x = element_text(angle = 0, hjust = 1),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+ scale_fill_grey() + guides(fill=FALSE)+scale_x_discrete(name = "Zircon roundness",limits=c("Euhedral","Subrounded","Rounded"))

hm_t = read.table("hm_t.txt", header = TRUE)
hm_tplot = ggplot(data = hm_t, aes(x=Tectonic_discrimination, y=Percentage)) + geom_boxplot(outlier.colour = NULL)+ theme_bw() + theme(axis.text.x = element_text(angle = 0, hjust = 1),axis.title.x=element_blank(),axis.title.y=element_blank(),panel.background = element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank())+ scale_fill_grey() + guides(fill=FALSE)+scale_x_discrete(name = "Tectonic discrimination",limits=c("Metamorphic","Magmatic","Ultramafic"))

multiplot(ccplot,  hm_tplot, hmplot, hrplot, cols=2)


summary(hm[1:2])

                     