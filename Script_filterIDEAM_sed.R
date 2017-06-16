#Scirpt to filtering ideam data

library("WriteXLS")
library("gtools")
library("plyr")
library("qpcR")
setwd("E:/Sedimentology/Project FINAL/Rproject/Rdirectory")

outputdirectory <-getwd()
#filenames <- list.files(pattern="*.csv")
#all_data <- readLines("NPEREZ06")
all_data <- readLines("NICOLPERECONSUEG")

#find all the lines with the word IDEAM

grep("I D E A M", all_data)

val_med <- grep("CONCENTRACION MEDIA DIARIA DE SEDIMENTOS EN SUSPENSION ",all_data)

all_data[val_med[1] + 12]

all_data[val_med[1] + 42]


#This loop goes step by step into all of the parts of the file that have val_med
for(i in 1:length(val_med)){
 
  
  
  #pruebba
  a<- data.frame(matrix(ncol = 13, nrow = 31))
  
  line <- val_med[i]+11
  
  
  
  for(j in 1:31){
    
    
    b <- all_data[line +j]
    
    #1
    
    a[j,1] <- substr(b, 12, 14)
    
    # 2 JANUARY
    if(substr(b, 21, 21)==" "){
      
      a[j,2] <- substr(b, 22, 25)
      
    } else {
      a[j,2] <- substr(b, 21, 25)
    }
    
    # 3 FEBRUARY
    
    if(substr(b, 30, 30)==" "){
      
      a[j,3] <- substr(b, 31, 34)
      
    } else {
      a[j,3] <- substr(b, 30, 34)
    }
    
    # 4 MARCH
    
    if(substr(b, 39, 39)==" "){
      
      a[j,4] <- substr(b, 40, 43)
      
    } else {
      a[j,4] <- substr(b, 39, 43)
    }
    
    # 5 APRIL
    
    if(substr(b, 48, 48)==" "){
      
      a[j,5] <- substr(b, 49, 52)
      
    } else {
      a[j,5] <- substr(b, 48, 52)
    }
    
    # 6 MAY
    
    if(substr(b, 57, 57)==" "){
      
      a[j,6] <- substr(b, 58, 61)
      
    } else {
      a[j,6] <- substr(b, 57, 61)
    }
    
    # 7 JUNIO
    
    if(substr(b, 66, 66)==" "){
      
      a[j,7] <- substr(b, 67, 70)
      
    } else {
      a[j,7] <- substr(b, 66, 70)
    }
    
    # 8 JULIO
    
    
    if(substr(b, 75, 75)==" "){
      
      a[j,8] <- substr(b, 76, 79)
      
    } else {
      a[j,8] <- substr(b, 75, 79)
    }
    
    # 9 AGOSTO
    
    if(substr(b, 84, 84)==" "){
      
      a[j,9] <- substr(b, 85, 88)
      
    } else {
      a[j,9] <- substr(b, 84, 88)
    }
    
    # 10 SEPTIEMBRE
    
    if(substr(b, 93, 93)==" "){
      
      a[j,10] <- substr(b, 94, 97)
      
    } else {
      a[j,10] <- substr(b, 93, 97)
    }
    
    # 11 OCTUBRE
    
    if(substr(b, 102, 102)==" "){
      
      a[j,11] <- substr(b, 103, 106)
      
    } else {
      a[j,11] <- substr(b, 102, 106)
    }
    
    # 12 NOVIEMBRE
    
    if(substr(b, 111, 111)==" "){
      
      a[j,12] <- substr(b, 112, 115)
      
    } else {
      a[j,12] <- substr(b, 111, 115)
    }
    
    # 13 DICIEMBRE
    
    if(substr(b, 120, 120)==" "){
      
      a[j,13] <- substr(b, 121, 124)
      
    } else {
      a[j,13] <- substr(b, 120, 1245)
    }
    
    
    
  }
  
  #here write a line to convert all a into numeric values with as.numeric(object)
  
  apply(a,1,function(x) as.numeric(x))-> a
  t(a) -> a
  
  
  
  #now I need to obtain information about what this data is about e.g., caud_med_di_YYYY_SSSSSS for caudales medios diarios
  #where s corresponds to the station number
  line2 <- val_med[i]+2
  
  text <- all_data[line2]
  
  year <- substr(text,60,63)
  
  station <- substr(text,105,112)
  
  name1 <- paste(c("conc_med_di_",year,"_",station,".txt"),collapse = '')
  
  vd = c(getwd(),'/',"filtered files",'/')
  directory = paste(vd,collapse = '')
  dir.create(directory,showWarnings = FALSE)
  
  name1 <- paste(c(directory,name1),collapse = '')

  write.table(a, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

  
}




