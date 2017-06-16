#Read and plot DATA

#station 25027050
setwd("E:/Sedimentology/Project FINAL/Rproject/Rdirectory/filtered files")

A.plot <- read.table("25027050caudales_diarios.txt",sep ="\t")

A.plot <- A.plot[-1,]

A.plot <- A.plot[A.plot$V1>2000,]

A.plot$V3 <- as.Date(A.plot$V3, "%Y_%m_%d")
#VALORES MEDIOS  DIARIOS DE CAUDALES (M3/Seg)
plot(A.plot$V3,A.plot$V4, t="p")


#station 25027640
A.plot <- read.table("25027640caudales_diarios.txt",sep ="\t")

A.plot <- A.plot[-1,]

A.plot <- A.plot[A.plot$V1>2000,]

A.plot$V3 <- as.Date(A.plot$V3, "%Y_%m_%d")
#VALORES MEDIOS  DIARIOS DE CAUDALES (M3/Seg)
plot(A.plot$V3,A.plot$V4, t="p")
