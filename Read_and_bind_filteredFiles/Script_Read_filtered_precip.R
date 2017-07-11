#Read the filtered files of level data


setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")


files = c("25025150","25020780","25020480")


for(k in 1: length(files)){
  
  #patter with specific station code, they will be arranged by age already!
  filenames <- list.files(pattern= paste( c("^prec_total_diaria_",files[k]), collapse="" ))

  
  
  
  # to obtain the age from the file one can do the following. For example for 1966 which is in the position 1 of the filenames list

  
  A <- data.frame(matrix(ncol = 4, nrow = 1))
  
  count = 0
  
  #rbind(A,A)
  
  
  for(h in 1:length(filenames)){
    
    year <- substr(filenames[h],28,31)
    
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
  name1 <- paste(c(station,"_precipitacion_total_diaria",".txt"),collapse = '')
  
  write.table(A, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)
  
  
  #A.subset <- A[A$X1>2000,]
  
  #A.subset$X3 <- as.Date(A.subset$X3, "%Y_%m_%d")
  
  #plot(A.subset$X3,A.subset$X4, t="p")
  
  
  
  

  
  
  
}


