#Script to read the previously generated filtered files 
#and produce 1 table with all the data per locality

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")

files = c("25027640","25027050","27037010","26247030","25027270")
  
for(k in 1: length(files)){
  
  #patter with specific station code, they will be arranged by age already!
  filenames <- list.files(pattern= paste( c("^caud_med_di_",files[k]), collapse=""))
  
  #Availabe stations (i.e., stations with data)
  #station = 25027640
  #station = 25027050
  #station = 27037010
  #station = 26247030
  #station = 25027270
  
  # to obtain the age from the file one can do the following. For example for 1966 which is in the position 1 of the filenames list
  
  A <- data.frame(matrix(ncol = 4, nrow = 1))
  
  count = 0
  
  #rbind(A,A)
  
  
  for(h in 1:length(filenames)){
    
    year <- substr(filenames[h],22,25)
    
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
  
  
  station <- files[k]
  name1 <- paste(c(station,"caudales_diarios",".txt"),collapse = '')
  
  write.table(A, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)
  
  
  
  
}