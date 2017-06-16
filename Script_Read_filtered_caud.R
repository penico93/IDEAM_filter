#Script to read the previously generated filtered files and produce 1 table with all the data per locality

setwd("E:/Sedimentology/Project FINAL/Rproject/Rdirectory/filtered files/")

#patter with specific station code, they will be arranged by age already!
filenames <- list.files(pattern="*26247030")

station = 26247030

# to obtain the age from the file one can do the following. For example for 1966 which is in the position 1 of the filenames list

substr(filenames[1],13,16)

A <- data.frame(matrix(ncol = 4, nrow = 1))

count = 0

#rbind(A,A)


for(h in 1:length(filenames)){
  
  year <- substr(filenames[h],13,16)
  
  data <- read.table(filenames[h])
  
  for(i in 1:31){
    
    
    for(j in 2:13){
      B <- data.frame(matrix(ncol = 4, nrow = 1))
      B[1,1] <- year
      B[1,2] <- count
      B[1,3] <- paste(c(year,"_",toString(j-1),"_",toString(i)),collapse = "")
      B[1,4] <- data[i,j]
      
      A <- rbind(A,B)
      
      count <- count + 1
    }
  
  
  }
  
  
}



name1 <- paste(c(station,"caudales_diarios",".txt"),collapse = '')

write.table(A, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)


A.subset <- A[A$X1>2000,]

A.subset$X3 <- as.Date(A.subset$X3, "%Y_%m_%d")

plot(A.subset$X3,A.subset$X4, t="p")


