# FILL NA'S IN THE TIME SERIES - precipitation {rainfall}

setwd("E:/Sedimentology/R hydrology/Rdirectory/Data/filtered files")
Sys.setlocale("LC_TIME", "English")

#load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(hydroTSM)

# START data.25020480
#read rainfall data - 25020480_precipitacion_total_diaria
data.25020480 <- read.table("25020480_precipitacion_total_diaria.txt",sep ="\t")

data.25020480.raw <- na.omit(data.25020480)
data.25020480.raw <- data.25020480.raw[data.25020480.raw$V1>1975,]
data.25020480.raw$V3 <- as.Date(data.25020480.raw$V3, "%Y_%m_%d")
data.25020480.raw <- data.25020480.raw[,-1]
data.25020480.raw <- data.25020480.raw[,-1]

#Loop to find NA's and replace with a constant value, in this case 222.9876
for(i in 1:length(data.25020480$V4)){
  
  if(is.na(data.25020480$V4[i]) == TRUE){
    
    data.25020480$V4[i] = 222.9876
  }
}

data.25020480 <- na.omit(data.25020480)
data.25020480 <- data.25020480[data.25020480$V1>1975,]
data.25020480$V3 <- as.Date(data.25020480$V3, "%Y_%m_%d")
data.25020480 <- data.25020480[,-1]
data.25020480 <- data.25020480[,-1]
#END data.25020480


# START data.25020780
#read rainfall data - 25020780_precipitacion_total_diaria
data.25020780 <- read.table("25020780_precipitacion_total_diaria.txt",sep ="\t")

data.25020780.raw <- na.omit(data.25020780)
data.25020780.raw <- data.25020780.raw[data.25020780.raw$V1>1975,]
data.25020780.raw$V3 <- as.Date(data.25020780.raw$V3, "%Y_%m_%d")
data.25020780.raw <- data.25020780.raw[,-1]
data.25020780.raw <- data.25020780.raw[,-1]


#Loop to find NA's and replace with a constant value, in this case 222.9876
for(i in 1:length(data.25020780$V4)){
  
  if(is.na(data.25020780$V4[i]) == TRUE){
    
    data.25020780$V4[i] = 222.9876
  }
}

data.25020780 <- na.omit(data.25020780)
data.25020780 <- data.25020780[data.25020780$V1>1975,]
data.25020780$V3 <- as.Date(data.25020780$V3, "%Y_%m_%d")
data.25020780 <- data.25020780[,-1]
data.25020780 <- data.25020780[,-1]


# a dataframe of four columns and length(dates.1986) or 365 number of rows to fill with NA's
# a sequence of all the years in  2011
dates.1986 <- seq( as.Date("1986_01_01","%Y_%m_%d"), as.Date("1986_12_31","%Y_%m_%d"), by="+1 day")

df.1986 <- data.frame(matrix(ncol = 4, nrow = length(dates.1986)))

for(d in 1:length(dates.1986)){
  
  df.1986[d,1] <- 1986
  df.1986[d,2] <- d
  df.1986[d,3] <- toString(dates.1986[d])
  df.1986[d,4] <- 222.9876
  
}



df.1986 <- df.1986[,-1]
df.1986 <- df.1986[,-1]
colnames(df.1986) <- colnames(data.25020780)
data.25020780 <- rbind(data.25020780,df.1986)
data.25020780 <- na.omit(data.25020780)

#END data.25020780

# START data.25025150
#read rainfall data - 25025150_precipitacion_total_diaria
data.25025150 <- read.table("25025150_precipitacion_total_diaria.txt",sep ="\t")

data.25025150.raw <- na.omit(data.25025150)
data.25025150.raw <- data.25025150.raw[data.25025150.raw$V1>1975,]
data.25025150.raw$V3 <- as.Date(data.25025150.raw$V3, "%Y_%m_%d")
data.25025150.raw <- data.25025150.raw[,-1]
data.25025150.raw <- data.25025150.raw[,-1]



#Loop to find NA's and replace with a constant value, in this case 222.9876
for(i in 1:length(data.25025150$V4)){
  
  if(is.na(data.25025150$V4[i]) == TRUE){
    
    data.25025150$V4[i] = 222.9876
  }
}

data.25025150 <- na.omit(data.25025150)
data.25025150 <- data.25025150[data.25025150$V1>1975,]
data.25025150$V3 <- as.Date(data.25025150$V3, "%Y_%m_%d")
data.25025150 <- data.25025150[,-1]
data.25025150 <- data.25025150[,-1]


#END data.25025150

colnames(data.25025150) <- c("Date","Precipitation")
colnames(data.25020480) <- c("Date","Precipitation")
colnames(data.25020780) <- c("Date","Precipitation")


#get statistics

#Here find monthly statistics for all of the data

colnames(data.25025150.raw) <- c("Date","Precipitation")
zoo.25025150<- read.zoo(data.25025150.raw, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25025150 <- daily2monthly(zoo.25025150, FUN=mean, na.rm=TRUE)
m.median.25025150 <- monthlyfunction(m.25025150, FUN=median, na.rm=TRUE)

colnames(data.25020480.raw) <- c("Date","Precipitation")
zoo.25020480<- read.zoo(data.25020480.raw, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25020480 <- daily2monthly(zoo.25020480, FUN=mean, na.rm=TRUE)
m.median.25020480 <- monthlyfunction(m.25020480, FUN=median, na.rm=TRUE)

colnames(data.25020780.raw) <- c("Date","Precipitation")
zoo.25020780<- read.zoo(data.25020780.raw, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25020780 <- daily2monthly(zoo.25020780, FUN=mean, na.rm=TRUE)
m.median.25020780 <- monthlyfunction(m.25020780, FUN=median, na.rm=TRUE)

#START FILTER


data.25020480 <- na.omit(data.25020480)
data.25025150 <- na.omit(data.25025150)
data.25020780 <- na.omit(data.25020780)


#start Loop for filtering the data of water Precipitation for station 25020480

for(j in 1:length(data.25020480$Precipitation)){
  
  
  if(data.25020480$Precipitation[j] == 222.9876){
    count1 = 0
    count2 = 0
    
    temp.date <- data.25020480[j,][[1]]
    
    
    # if this date exists
    if(temp.date %in% data.25020780$Date == TRUE){
      
      #if this date exists in another file and is different from 222.9876
      if(data.25020780[data.25020780$Date == temp.date,][[2]]!=222.9876){
        
        dis.25020780 <- data.25020780[data.25020780$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25020780 <- m.median.25020780[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    if(temp.date %in% data.25025150$Date == TRUE){
      
      if(data.25025150[data.25025150$Date == temp.date,][[2]]!=222.9876){
        
        dis.25025150 <- data.25025150[data.25025150$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25025150 <- m.median.25025150[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    
    if(count2 == 2){
      
      data.25020480$Precipitation[j]  <- (m.median.25020480[m.num][[1]])*(((dis.25020780/(m.median.25020780[m.num][[1]]))+(dis.25025150/(m.median.25025150[m.num][[1]])))/(count2))
      
    } else {
      
      data.25020480$Precipitation[j]   <- m.median.25020480[m.num][[1]]
      
    }
    
    
  }
  
}


for(j in 1:length(data.25020480$Precipitation)){
  
  if(data.25020480$Precipitation[j] == 222.9876){
    
    temp.date <- data.25020480[j,][[1]]
    m.num <- months.Date(temp.date, abbreviate = TRUE)
    
    data.25020480$Precipitation[j] <- m.median.25020480[m.num][[1]]
    
    
  }
  
  
}

#update statistics after filtering
colnames(data.25020480) <- c("Date","Precipitation")
zoo.25020480<- read.zoo(data.25020480, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25020480 <- daily2monthly(zoo.25020480, FUN=mean, na.rm=TRUE)
m.median.25020480 <- monthlyfunction(m.25020480, FUN=median, na.rm=TRUE)




data.25020480 <- na.omit(data.25020480)
data.25025150 <- na.omit(data.25025150)
data.25020780 <- na.omit(data.25020780)



#start Loop for filtering the data of water Precipitation for station 25025150

for(j in 1:length(data.25025150$Precipitation)){
  
  
  if(data.25025150$Precipitation[j] == 222.9876){
    count1 = 0
    count2 = 0
    
    temp.date <- data.25025150[j,][[1]]
    
    
    # if this date exists
    if(temp.date %in% data.25020780$Date == TRUE){
      
      #if this date exists in another file and is different from 222.9876
      if(data.25020780[data.25020780$Date == temp.date,][[2]]!=222.9876){
        
        dis.25020780 <- data.25020780[data.25020780$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25020780 <- m.median.25020780[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    if(temp.date %in% data.25020480$Date == TRUE){
      
      if(data.25020480[data.25020480$Date == temp.date,][[2]]!=222.9876){
        
        dis.25020480 <- data.25020480[data.25020480$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25020480 <- m.median.25020480[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    
    if(count2 == 2){
      
      data.25025150$Precipitation[j]  <- (m.median.25025150[m.num][[1]])*(((dis.25020780/(m.median.25020780[m.num][[1]]))+(dis.25020480/(m.median.25020480[m.num][[1]])))/(count2))
      
    } else {
      
      data.25025150$Precipitation[j]   <- m.median.25025150[m.num][[1]]
      
    }
    
    
  }
  
}


for(j in 1:length(data.25025150$Precipitation)){
  if(data.25025150$Precipitation[j] == 222.9876){
    
    temp.date <- data.25025150[j,][[1]]
    m.num <- months.Date(temp.date, abbreviate = TRUE)
    
    data.25025150$Precipitation[j] <- m.median.25025150[m.num][[1]]
    
    
  }
  
  
}


#update statistics after filtering

colnames(data.25025150) <- c("Date","Precipitation")
zoo.25025150<- read.zoo(data.25025150, format = "%Y-%m-%d", header = TRUE,sep = "/t")
m.25025150 <- daily2monthly(zoo.25025150, FUN=mean, na.rm=TRUE)
m.median.25025150 <- monthlyfunction(m.25025150, FUN=median, na.rm=TRUE)


###

data.25020480 <- na.omit(data.25020480)
data.25025150 <- na.omit(data.25025150)
data.25020780 <- na.omit(data.25020780)

#start Loop for filtering the data of water Precipitation for station 25020780

for(j in 1:length(data.25020780$Precipitation)){
  
  
  if(data.25020780$Precipitation[j] == 222.9876){
    count1 = 0
    count2 = 0
    
    temp.date <- data.25020780[j,][[1]]
    
    
    # if this date exists
    if(temp.date %in% data.25025150$Date == TRUE){
      
      #if this date exists in another file and is different from 222.9876
      if(data.25025150[data.25025150$Date == temp.date,][[2]]!=222.9876){
        
        dis.25025150 <- data.25025150[data.25025150$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25025150 <- m.median.25025150[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    if(temp.date %in% data.25020480$Date == TRUE){
      
      if(data.25020480[data.25020480$Date == temp.date,][[2]]!=222.9876){
        
        dis.25020480 <- data.25020480[data.25020480$Date == temp.date,][[2]]
        count2 = count2 + 1
        
      } else{
        m.num <- months.Date(temp.date, abbreviate = TRUE)
        
        dis.25020480 <- m.median.25020480[m.num][[1]]
        count2 = count2 + 1
        
        
      }
      
    }
    
    
    
    if(count2 == 2){
      
      data.25020780$Precipitation[j]  <- (m.median.25020780[m.num][[1]])*(((dis.25025150/(m.median.25025150[m.num][[1]]))+(dis.25020480/(m.median.25020480[m.num][[1]])))/(count2))
      
    } else {
      
      data.25020780$Precipitation[j]   <- m.median.25020780[m.num][[1]]
      
    }
    
    
  }
  
}


for(j in 1:length(data.25020780$Precipitation)){
  if(data.25020780$Precipitation[j] == 222.9876){
    
    temp.date <- data.25020780[j,][[1]]
    m.num <- months.Date(temp.date, abbreviate = TRUE)
    
    data.25020780$Precipitation[j] <- m.median.25020780[m.num][[1]]
    
    
  }
  
  
}


#### save filtered files


vd = c(getwd(),'/',"filter_and_fill",'/')
directory = paste(vd,collapse = '')
dir.create(directory,showWarnings = FALSE)



name1 <- paste(c("filled_precipitation_","25020480",".txt"),collapse = '')

name1 <- paste(c(directory,name1),collapse = '')

write.table(data.25020480, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

name1 <- paste(c("filled_precipitation_","25025150",".txt"),collapse = '')

name1 <- paste(c(directory,name1),collapse = '')

write.table(data.25025150, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)

name1 <- paste(c("filled_precipitation_","25020780",".txt"),collapse = '')

name1 <- paste(c(directory,name1),collapse = '')

write.table(data.25020780, file = name1, sep ="\t",row.names=FALSE,col.names=FALSE)







