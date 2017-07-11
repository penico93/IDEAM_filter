# FILL NA'S IN THE TIME SERIES of water discharge

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")

#load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(hydroTSM)

# START data.25027050
#read water discharge from file
data.25027050 <- read.table("25027050caudales_diarios.txt",sep ="\t")


#Loop to find NA's and replace with a constant value, in this case 5003 was chosen
for(i in 1:length(data.25027050$V4)){
  
  if(is.na(data.25027050$V4[i]) == TRUE){
    
    data.25027050$V4[i] = 5003
  }
  
  
}

#erase 
data.25027050 <- na.omit(data.25027050)

data.25027050 <- data.25027050[data.25027050$V1>1975,]

data.25027050$V3 <- as.Date(data.25027050$V3, "%Y_%m_%d")

# a sequence of all the years in  2011
dates.2011 <- seq( as.Date("2011_01_01","%Y_%m_%d"), as.Date("2011_12_31","%Y_%m_%d"), by="+1 day")

# a dataframe of four columns and length(dates.2011) or 365 number of rows
df.2011 <- data.frame(matrix(ncol = 4, nrow = length(dates.2011)))

for(d in 1:length(dates.2011)){
  
  df.2011[d,1] <- 2011
  df.2011[d,2] <- d
  df.2011[d,3] <- toString(dates.2011[d])
  df.2011[d,4] <- 5003
  
}



colnames(df.2011) <- colnames(data.25027050)

data.25027050 <- rbind(data.25027050,df.2011)
data.25027050 <- data.25027050[,-1]
data.25027050 <- data.25027050[,-1]

data.25027050 <- na.omit(data.25027050)

# END data.25027050


#START data.26247030

#STATION 26247030
#Apavi upper cauca river
# reading files and dealing with NA's

data.26247030 <- read.table("26247030caudales_diarios.txt",sep ="\t")



for(i in 1:length(data.26247030$V4)){
  
  if(is.na(data.26247030$V4[i]) == TRUE){
    
    data.26247030$V4[i] = 5003
  }
  
}

data.26247030 <- na.omit(data.26247030)

data.26247030 <- data.26247030[data.26247030$V1>1975,]

data.26247030$V3 <- as.Date(data.26247030$V3, "%Y_%m_%d")

data.26247030 <- data.26247030[,-1]
data.26247030 <- data.26247030[,-1]

data.26247030 <- na.omit(data.26247030)

#FIN



#END data.26247030



#START data.25027270

# STATION 25027270
# reading files and dealing with NA's

data.25027270 <- read.table("25027270caudales_diarios.txt",sep ="\t")
for(i in 1:length(data.25027270$V4)){
  
  if(is.na(data.25027270$V4[i]) == TRUE){
    
    data.25027270$V4[i] = 5003
  }
  
}

data.25027270 <- na.omit(data.25027270)

data.25027270 <- data.25027270[data.25027270$V1>1975,]

data.25027270$V3 <- as.Date(data.25027270$V3, "%Y_%m_%d")

data.25027270 <- data.25027270[,-1]
data.25027270 <- data.25027270[,-1]

data.25027270 <- na.omit(data.25027270)


# FIN


#END data.25027270




#Here find monthly statistics for all of the data

#assign specific names to columns of each dataframe
colnames(data.25027050) <- c("Date","Discharge")
colnames(data.25027270) <- c("Date","Discharge")
colnames(data.26247030) <- c("Date","Discharge")

zoo.25027050<- read.zoo(data.25027050, format = "%Y-%m-%d", header = TRUE,sep = "/t")
zoo.25027270<- read.zoo(data.25027270, format = "%Y-%m-%d", header = TRUE,sep = "/t")
zoo.26247030<- read.zoo(data.26247030, format = "%Y-%m-%d", header = TRUE,sep = "/t")

m.25027050 <- daily2monthly(zoo.25027050, FUN=mean, na.rm=TRUE)
m.median.25027050 <- monthlyfunction(m.25027050, FUN=median, na.rm=TRUE)

m.25027270 <- daily2monthly(zoo.25027270, FUN=mean, na.rm=TRUE)
m.median.25027270 <- monthlyfunction(m.25027270, FUN=median, na.rm=TRUE)

m.26247030 <- daily2monthly(zoo.26247030, FUN=mean, na.rm=TRUE)
m.median.26247030 <- monthlyfunction(m.26247030, FUN=median, na.rm=TRUE)




#start Loop for filtering the data of water discharge for station 25027050

for(j in 1:length(data.25027050$Discharge)){
  
  
  if(data.25027050$Discharge[j] == 5003){
    count1 = 0
    count2 = 0
    
    temp.date <- data.25027050[j,][[1]]

    
    # if this date exists
    if(temp.date %in% data.26247030$Date == TRUE){
      
      #if this date exists in another file and is different from 5003
      if(data.26247030[data.26247030$Date == temp.date,][[2]]!=5003){
        
        dis.26247030 <- data.26247030[data.26247030$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.26247030 <- m.median.26247030[m.num][[1]]
        count2 = count2 + 1
        
        
      }
  
    }
    
    
    if(temp.date %in% data.25027270$Date == TRUE){
      
      if(data.25027270[data.25027270$Date == temp.date,][[2]]!=5003){
        
        dis.25027270 <- data.25027270[data.25027270$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25027270 <- m.median.25027270[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    
    if(count2 == 2){
      
      data.25027050$Discharge[j]  <- (m.median.25027050[m.num][[1]])*(((dis.26247030/(m.median.26247030[m.num][[1]]))+(dis.25027270/(m.median.25027270[m.num][[1]])))/(count2))
      
    } else {
      
      data.25027050$Discharge[j]   <- m.median.25027050[m.num][[1]]
      
    }
    
    
  }
  
}


for(j in 1:length(data.25027050$Discharge)){
  if(data.25027050$Discharge[j] == 5003){
    
    temp.date <- data.25027050[j,][[1]]
    m.num <- months.Date(temp.date, abbreviate = TRUE)
    
    data.25027050$Discharge[j] <- m.median.25027050[m.num][[1]]
    
    
  }
  
  
}

#REFRESH DATA FROM 25027050
zoo.25027050<- read.zoo(data.25027050, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25027050 <- daily2monthly(zoo.25027050, FUN=mean, na.rm=TRUE)
m.median.25027050 <- monthlyfunction(m.25027050, FUN=median, na.rm=TRUE)

#end of Loop for filtering the data of water discharge for station 25027050



#start Loop for filtering the data of water discharge for station 26247030

for(j in 1:length(data.26247030$Discharge)){
  
  
  if(data.26247030$Discharge[j] == 5003){
    count1 = 0
    count2 = 0
    
    temp.date <- data.26247030[j,][[1]]
    
    
    # if this date exists
    if(temp.date %in% data.25027050$Date == TRUE){
      
      #if this date exists in another file and is different from 5003
      if(data.25027050[data.25027050$Date == temp.date,][[2]]!=5003){
        
        dis.25027050 <- data.25027050[data.25027050$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25027050 <- m.median.25027050[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    if(temp.date %in% data.25027270$Date == TRUE){
      
      if(data.25027270[data.25027270$Date == temp.date,][[2]]!=5003){
        
        dis.25027270 <- data.25027270[data.25027270$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25027270 <- m.median.25027270[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    
    if(count2 == 2){
      
      data.26247030$Discharge[j]  <- (m.median.26247030[m.num][[1]])*(((dis.25027050/(m.median.25027050[m.num][[1]]))+(dis.25027270/(m.median.25027270[m.num][[1]])))/(count2))
      
    } else {
      
      data.26247030$Discharge[j]   <- m.median.26247030[m.num][[1]]
      
    }
    
    
  }
  
}


for(j in 1:length(data.26247030$Discharge)){
  if(data.26247030$Discharge[j] == 5003){
    
    temp.date <- data.26247030[j,][[1]]
    m.num <- months.Date(temp.date, abbreviate = TRUE)
    
    data.26247030$Discharge[j] <- m.median.26247030[m.num][[1]]
    
    
  }
  
  
}
#REFRESH DATA FROM 26247030
zoo.26247030 <- read.zoo(data.26247030, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.26247030 <- daily2monthly(zoo.26247030, FUN=mean, na.rm=TRUE)
m.median.26247030 <- monthlyfunction(m.26247030, FUN=median, na.rm=TRUE)

#end of Loop for filtering the data of water discharge for station 26247030





#start Loop for filtering the data of water discharge for station 25027270

for(j in 1:length(data.25027270$Discharge)){
  
  
  if(data.25027270$Discharge[j] == 5003){
    count1 = 0
    count2 = 0
    
    temp.date <- data.25027270[j,][[1]]
    
    
    # if this date exists
    if(temp.date %in% data.25027050$Date == TRUE){
      
      #if this date exists in another file and is different from 5003
      if(data.25027050[data.25027050$Date == temp.date,][[2]]!=5003){
        
        dis.25027050 <- data.25027050[data.25027050$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25027050 <- m.median.25027050[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    if(temp.date %in% data.26247030$Date == TRUE){
      
      if(data.26247030[data.26247030$Date == temp.date,][[2]]!=5003){
        
        dis.26247030 <- data.26247030[data.26247030$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.26247030 <- m.median.26247030[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    
    if(count2 == 2){
      
      data.25027270$Discharge[j]  <- (m.median.25027270[m.num][[1]])*(((dis.25027050/(m.median.25027050[m.num][[1]]))+(dis.26247030/(m.median.26247030[m.num][[1]])))/(count2))
      
    } else {
      
      data.25027270$Discharge[j]   <- m.median.25027270[m.num][[1]]
      
    }
    
    
  }
  
}


for(j in 1:length(data.25027270$Discharge)){
  if(data.25027270$Discharge[j] == 5003){
    
    temp.date <- data.25027270[j,][[1]]
    m.num <- months.Date(temp.date, abbreviate = TRUE)
    
    data.25027270$Discharge[j] <- m.median.25027270[m.num][[1]]
    
    
  }
  
  
}

#end of Loop for filtering the data of water discharge for station 25027270







vd = c(getwd(),'/',"filter_and_fill",'/')
directory = paste(vd,collapse = '')
dir.create(directory,showWarnings = FALSE)



name1 <- paste(c("filled_caud_","25027050",".txt"),collapse = '')

name1 <- paste(c(directory,name1),collapse = '')

write.table(data.25027050, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

name1 <- paste(c("filled_caud_","26247030",".txt"),collapse = '')

name1 <- paste(c(directory,name1),collapse = '')

write.table(data.26247030, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

name1 <- paste(c("filled_caud_","25027270",".txt"),collapse = '')

name1 <- paste(c(directory,name1),collapse = '')

write.table(data.25027270, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)




