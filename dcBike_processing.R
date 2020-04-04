#################################################################################################

## This script loads in and processed DC Capitol Bike Share trip data from Q4, 2010 onwards. 
# source: https://s3.amazonaws.com/capitalbikeshare-data/index.html


# The following datasets are created:

  # Trips dataset - 22m records of individual trips taken 
  # Stations dataset - static data for ~580 bike stations
  # Station_to_station dataset - aggregated data for trips grouped by year, day, member type, etc
  # Daily trips dataset - total number of trips by day
  # Bike info dataset - data on total trips, duration, by bike number and year


#################################################################################################



# Set Up ----


library(tidyverse)
library(prophet)
library(lubridate)


# Unzipping, Reading in CSVs ----

# Unzipping files
  # setwd("/home/ravi/Documents/DC BikeShare Data/DC Bike Share/Zipped Files")
  # zipped_files<-list.files()
  # for (i in 1:length(zipped_files)) {
  #   unzip(zipped_files[i],exdir = "/home/ravi/Documents/DC BikeShare Data/DC Bike Share/unzipped")
  # }
    
# Reading in .CSVs
  setwd("/home/ravi/Documents/DC BikeShare Data/DC Bike Share/unzipped")
  unzipped_files<-list.files()
  bike_Share<-lapply(unzipped_files, function(x) read.csv(x,stringsAsFactors = F,header = T))
  
  
  #save(bike_Share,file="bike_share_unprocessed.rdata")


# Formatting Before joining----

# Extensive formatting of the data is necessary. Sinc 2010 the data collected has changed significantly
  # Different number variables reported 
  # Different format of variables 
  # Minor changes in variable names

  # Approach : Read in sequentially, the data is processed in chunks
  
# Number of variables included changes over time
  # var names that change: Bike.number ~ Bike. & Member.Type ~ Subscription.Type
  #for (i in 1:length(bike_Share)) print(c(i,bike_Share[[i]] %>% ncol())) 
  
  
# creating list for output of processed bike_share elements
bike_Share_f <- list()
    

# Cleaning the first five files
      
for (i in 1:5){
      
  dc_bike1<-bike_Share[[i]] 
      
  # subsetting out station number
  dc_bike1$Start.station.number <- 
    substr(dc_bike1$Start.station,nchar(dc_bike1$Start.station)-5,nchar(dc_bike1$Start.station)-1)
  dc_bike1$End.station.number <- 
    substr(dc_bike1$End.station,nchar(dc_bike1$End.station)-5,nchar(dc_bike1$End.station)-1)
  
  dc_bike1$Start.station.number <- as.numeric(dc_bike1$Start.station.number)
  dc_bike1$End.station.number <- as.numeric(dc_bike1$End.station.number)
      
  # removing station number from station names
  dc_bike1$Start.station <- 
    substr(dc_bike1$Start.station,1,nchar(dc_bike1$Start.station)-7)
  dc_bike1$End.station <-
    substr(dc_bike1$End.station,1,nchar(dc_bike1$End.station)-7)
      
  # Formatting dates
  dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format = '%m/%d/%Y %H:%M')
  dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
      
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
      
  dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
      
  
  
  ## Fixing Durations
  
  dc_bike1$H <- as.numeric(str_split_fixed(dc_bike1$Duration,pattern = "h",n = 2)[,1])
  
  dc_bike1$M <- str_split_fixed(dc_bike1$Duration,pattern = "m",n = 2)[,1]
  dc_bike1$M <- substr(dc_bike1$M,nchar(dc_bike1$M)-1,nchar(dc_bike1$M))
  dc_bike1$M <- str_trim(dc_bike1$M,"both")
  dc_bike1$M <- as.numeric(dc_bike1$M)
  
  dc_bike1$Sec <- str_split_fixed(dc_bike1$Duration,pattern = "s",n = 2)[,1]
  dc_bike1$Sec <- substr(dc_bike1$Sec,nchar(dc_bike1$Sec)-1,nchar(dc_bike1$Sec))
  dc_bike1$Sec <- str_trim(dc_bike1$Sec,"both")
  dc_bike1$Sec <- as.numeric(dc_bike1$Sec)
  
  # re-calculating duration in seconds
  dc_bike1$Duration <- dc_bike1$H*60*60+dc_bike1$M*60+dc_bike1$Sec
  
  dc_bike1 <- dc_bike1 %>% select(-M,-H,-Sec)
  
  # re-naming
  names(dc_bike1)[4]<-"Bike.number"
  names(dc_bike1)[5]<-"Member.type"
  
  
  bike_Share_f[[i]] <- dc_bike1
}
     



# Cleaning 6th to 19th files. Station numbers are no longer included

for (i in 6:10){
  
  dc_bike1<-bike_Share[[i]] 
  
  # Putting in placeholder 'NA' variable for station numbers 
  dc_bike1$Start.station.number <- NA
  dc_bike1$End.station.number <- NA

  # Formatting dates
  dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format = '%m/%d/%Y %H:%M')
  dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
  
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
  
  dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
  
  
  ## Fixing Durations
  
  dc_bike1$H <- as.numeric(str_split_fixed(dc_bike1$Duration,pattern = "h",n = 2)[,1])
  
  dc_bike1$M <- str_split_fixed(dc_bike1$Duration,pattern = "m",n = 2)[,1]
  dc_bike1$M <- substr(dc_bike1$M,nchar(dc_bike1$M)-1,nchar(dc_bike1$M))
  dc_bike1$M <- str_trim(dc_bike1$M,"both")
  dc_bike1$M <- as.numeric(dc_bike1$M)
  
  dc_bike1$Sec <- str_split_fixed(dc_bike1$Duration,pattern = "s",n = 2)[,1]
  dc_bike1$Sec <- substr(dc_bike1$Sec,nchar(dc_bike1$Sec)-1,nchar(dc_bike1$Sec))
  dc_bike1$Sec <- str_trim(dc_bike1$Sec,"both")
  dc_bike1$Sec <- as.numeric(dc_bike1$Sec)
  
  # re-calculating duration in seconds
  dc_bike1$Duration <- dc_bike1$H*60*60+dc_bike1$M*60+dc_bike1$Sec
  
  dc_bike1 <- dc_bike1 %>% select(-M,-H,-Sec)
  
  # renaming
  names(dc_bike1)[4]<-"Bike.number"
  names(dc_bike1)[5]<-"Member.type"
  
  bike_Share_f[[i]] <- dc_bike1
}



# For the 11th file 'Start.date' changed to 'Start.TIME'

for (i in 11){
  
  dc_bike1<-bike_Share[[i]] 
  
  # Putting in placeholder 'NA' variable for station numbers 
  dc_bike1$Start.station.number <- NA
  dc_bike1$End.station.number <- NA
  
  # Formatting dates
  dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.time, format='%m/%d/%Y %H:%M')
  dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
  
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
  
  dc_bike1<-dc_bike1 %>% select(-Start.time,-End.date)
  
  
  ## Fixing Durations
  
  dc_bike1$H <- as.numeric(str_split_fixed(dc_bike1$Duration,pattern = "h",n = 2)[,1])
  
  dc_bike1$M <- str_split_fixed(dc_bike1$Duration,pattern = "m",n = 2)[,1]
  dc_bike1$M <- substr(dc_bike1$M,nchar(dc_bike1$M)-1,nchar(dc_bike1$M))
  dc_bike1$M <- str_trim(dc_bike1$M,"both")
  dc_bike1$M <- as.numeric(dc_bike1$M)
  
  dc_bike1$Sec <- str_split_fixed(dc_bike1$Duration,pattern = "s",n = 2)[,1]
  dc_bike1$Sec <- substr(dc_bike1$Sec,nchar(dc_bike1$Sec)-1,nchar(dc_bike1$Sec))
  dc_bike1$Sec <- str_trim(dc_bike1$Sec,"both")
  dc_bike1$Sec <- as.numeric(dc_bike1$Sec)
  
  # re-calculating duration in seconds
  dc_bike1$Duration <- dc_bike1$H*60*60+dc_bike1$M*60+dc_bike1$Sec
  
  dc_bike1 <- dc_bike1 %>% select(-M,-H,-Sec)
  
  # renaming
  names(dc_bike1)[4]<-"Bike.number"
  names(dc_bike1)[5]<-"Member.type"
  bike_Share_f[[i]] <- dc_bike1
}




# Cleaning the 12th to the 15th files
for (i in c(12:15)){
  
  dc_bike1<-bike_Share[[i]] 
  
  # Putting in placeholder 'NA' variable for station numbers 
  dc_bike1$Start.station.number <- NA
  dc_bike1$End.station.number <- NA
  
  # Formatting dates
  dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format='%m/%d/%Y %H:%M')
  dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
  
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
  
  dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
  
  
  ## Fixing Durations
  
  dc_bike1$H <- as.numeric(str_split_fixed(dc_bike1$Duration,pattern = "h",n = 2)[,1])
  
  dc_bike1$M <- str_split_fixed(dc_bike1$Duration,pattern = "m",n = 2)[,1]
  dc_bike1$M <- substr(dc_bike1$M,nchar(dc_bike1$M)-1,nchar(dc_bike1$M))
  dc_bike1$M <- str_trim(dc_bike1$M,"both")
  dc_bike1$M <- as.numeric(dc_bike1$M)
  
  dc_bike1$Sec <- str_split_fixed(dc_bike1$Duration,pattern = "s",n = 2)[,1]
  dc_bike1$Sec <- substr(dc_bike1$Sec,nchar(dc_bike1$Sec)-1,nchar(dc_bike1$Sec))
  dc_bike1$Sec <- str_trim(dc_bike1$Sec,"both")
  dc_bike1$Sec <- as.numeric(dc_bike1$Sec)
  
  # re-calculating duration in seconds
  dc_bike1$Duration <- dc_bike1$H*60*60+dc_bike1$M*60+dc_bike1$Sec
  
  dc_bike1 <- dc_bike1 %>% select(-M,-H,-Sec)
  
  # renaming
  
  names(dc_bike1)[1]<-"Duration"
  names(dc_bike1)[4]<-"Bike.number"
  names(dc_bike1)[5]<-"Member.type"
  bike_Share_f[[i]] <- dc_bike1
}


# For the 16th 
  
for (i in 16){
    
    
    dc_bike1<-bike_Share[[i]] 
    
    # Putting in placeholder 'NA' variable for station numbers 
    dc_bike1$Start.station.number <- NA
    dc_bike1$End.station.number <- NA
    
    # Formatting dates
    dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format='%m/%d/%Y %H:%M')
    dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
    
    dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
    dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
    
    dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
    
    
    ## Fixing Durations
    
    dc_bike1$H <- as.numeric(str_split_fixed(dc_bike1$Duration,pattern = "h",n = 2)[,1])
    
    dc_bike1$M <- str_split_fixed(dc_bike1$Duration,pattern = "m",n = 2)[,1]
    dc_bike1$M <- substr(dc_bike1$M,nchar(dc_bike1$M)-1,nchar(dc_bike1$M))
    dc_bike1$M <- str_trim(dc_bike1$M,"both")
    dc_bike1$M <- as.numeric(dc_bike1$M)
    
    dc_bike1$Sec <- str_split_fixed(dc_bike1$Duration,pattern = "s",n = 2)[,1]
    dc_bike1$Sec <- substr(dc_bike1$Sec,nchar(dc_bike1$Sec)-1,nchar(dc_bike1$Sec))
    dc_bike1$Sec <- str_trim(dc_bike1$Sec,"both")
    dc_bike1$Sec <- as.numeric(dc_bike1$Sec)
    
    # re-calculating duration in seconds
    dc_bike1$Duration <- dc_bike1$H*60*60+dc_bike1$M*60+dc_bike1$Sec
    
    dc_bike1 <- dc_bike1 %>% select(-M,-H,-Sec)
    
    # renaming
    
    names(dc_bike1)[1]<-"Duration"
    names(dc_bike1)[4]<-"Bike.number"
    names(dc_bike1)[5]<-"Member.type"
    bike_Share_f[[i]] <- dc_bike1
    
  }
  
# For the 17th 
  
  for (i in 17){
    
    
    dc_bike1<-bike_Share[[i]] 
    
    # Putting in placeholder 'NA' variable for station numbers 
    dc_bike1$Start.station.number <- NA
    dc_bike1$End.station.number <- NA
    
    # Formatting dates
    dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format='%Y-%m-%d %H:%M')
    dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%Y-%m-%d %H:%M')
    
    dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
    dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
    
    dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
    
    
    ## Fixing Durations
    
    dc_bike1$H <- as.numeric(str_split_fixed(dc_bike1$Duration,pattern = "h",n = 2)[,1])
    
    dc_bike1$M <- str_split_fixed(dc_bike1$Duration,pattern = "m",n = 2)[,1]
    dc_bike1$M <- substr(dc_bike1$M,nchar(dc_bike1$M)-1,nchar(dc_bike1$M))
    dc_bike1$M <- str_trim(dc_bike1$M,"both")
    dc_bike1$M <- as.numeric(dc_bike1$M)
    
    dc_bike1$Sec <- str_split_fixed(dc_bike1$Duration,pattern = "s",n = 2)[,1]
    dc_bike1$Sec <- substr(dc_bike1$Sec,nchar(dc_bike1$Sec)-1,nchar(dc_bike1$Sec))
    dc_bike1$Sec <- str_trim(dc_bike1$Sec,"both")
    dc_bike1$Sec <- as.numeric(dc_bike1$Sec)
    
    # re-calculating duration in seconds
    dc_bike1$Duration <- dc_bike1$H*60*60+dc_bike1$M*60+dc_bike1$Sec
    
    dc_bike1 <- dc_bike1 %>% select(-M,-H,-Sec)
    
    # renaming
    
    names(dc_bike1)[1]<-"Duration"
    names(dc_bike1)[4]<-"Bike.number"
    names(dc_bike1)[5]<-"Member.type"
    bike_Share_f[[i]] <- dc_bike1
    
  }
  
  
  
  
  
# Cleaning the 18th to the 19th files 
for (i in 18:19){
  
  dc_bike1<-bike_Share[[i]] 
  
  # Putting in placeholder 'NA' variable for station numbers 
  dc_bike1$Start.station.number <- NA
  dc_bike1$End.station.number <- NA
  
  # Formatting dates
  dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format='%m/%d/%Y %H:%M')
  dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
  
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
  
  dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
  
  # renaming
    
  names(dc_bike1)[1]<-"Duration"
  names(dc_bike1)[4]<-"Bike.number"
  names(dc_bike1)[5]<-"Member.type"
  bike_Share_f[[i]] <- dc_bike1
}



# Cleaning 20th to 26th. Station numbers and station names are broken apart again - just need to format dates

for (i in 20:26){
  
  dc_bike1<-bike_Share[[i]] 
  
  # Formatting dates
  dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format='%m/%d/%Y %H:%M')
  dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format = '%m/%d/%Y %H:%M')
  
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
  
  dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
  
  
  # re-ordering
  colOrdersMatch <- c(1,3,5,6,7,2,4,8,9,10,11)
  
  dc_bike1 <- dc_bike1[,colOrdersMatch]
  
  
  # re-naming
  names(dc_bike1)[1]<-"Duration"
  names(dc_bike1)[4]<-"Bike.number"
  
  
  bike_Share_f[[i]] <- dc_bike1
}

# Cleaning 27th to 39th. Format dates which have extra time stamps


#for (i in 27:39) {
#  print(class(bike_Share[[i]]$Start.date))
#  print(class(bike_Share[[i]]$End.date))
#}


for (i in 27:39){
  
  dc_bike1<-bike_Share[[i]] 
  
  # Formatting dates
  #dc_bike1$Start.date2 <- lubridate::as_datetime(dc_bike1$Start.date, format='%Y-%d-%m %H:%M:%S')
  #dc_bike1$End.date2 <- lubridate::as_datetime(dc_bike1$End.date, format='%Y-%d-%m %H:%M:%S')
  
  dc_bike1$Start.date2 <- as.Date(substr(dc_bike1$Start.date,1,10), format="%Y-%m-%d")
  dc_bike1$End.date2 <- as.Date(substr(dc_bike1$End.date,1,10), format="%Y-%m-%d")
    
  dc_bike1$Start_hr <- lubridate::hour(dc_bike1$Start.date2)
  dc_bike1$End_hr <- lubridate::hour(dc_bike1$End.date2)      
  
  dc_bike1<-dc_bike1 %>% select(-Start.date,-End.date)
  
  
  # re-ordering
  colOrdersMatch <- c(1,3,5,6,7,2,4,8,9,10,11)
  
  dc_bike1 <- dc_bike1[,colOrdersMatch]
  
  
  # re-naming
  names(dc_bike1)[1]<-"Duration"
  names(dc_bike1)[4]<-"Bike.number"
  
  
  bike_Share_f[[i]] <- dc_bike1
}

    

### Resolving column names ----
# There are some small differences to be resolved before row-binding

#for (i in 1:length(bike_Share_f)) print(names(bike_Share_f[[i]]))
    
    
column_names <-bike_Share_f[[1]] %>% names()
#for (i in 1:length(bike_Share_f)) print(column_names==names((bike_Share_f[[i]])))
  
for (i in 1:length(bike_Share_f)){
  names(bike_Share_f[[i]]) <- column_names
}


  
  save(bike_Share_f,file = "Pre_date_fix_df.rdata")
  
## Checking/Fixing Dates ----
  
  load(file = "Pre_date_fix_df.rdata")
  
  # No more NAs!
  for (i in 1:39) print(c(i,table(is.na(bike_Share_f[[i]]$Start.date2))))
  for (i in 1:39) print(c(i,table(is.na(bike_Share_f[[i]]$End.date2))))
  
  # Class divided between POSIXt and Date
  for (i in 1:39) print(c(i,class(bike_Share_f[[i]]$Start.date2)))
  for (i in 1:39) print(c(i,class(bike_Share_f[[i]]$End.date2)))
  
  
  
  # Changing dates from POSlXlt to Dates 

bike_Share_f2 <- list()

for (i in 1:26){
  
  dc_bike1<-bike_Share_f[[i]] 

  dc_bike1$Start.date2 <- lubridate::as_date(dc_bike1$Start.date2)
  dc_bike1$End.date2 <- lubridate::as_date(dc_bike1$End.date2)
  
  bike_Share_f2[[i]] <- dc_bike1
}

for (i in 27:39) {
  bike_Share_f2[[i]] <- bike_Share_f[[i]]
}

  # Checking Classes
  for (i in 1:39) print(c(i,class(bike_Share_f2[[i]]$Start.date2)))
  for (i in 1:39) print(c(i,class(bike_Share_f2[[i]]$End.date2)))

  # Checking NAs
  for (i in 1:39) print(c(i,table(is.na(bike_Share_f2[[i]]$Start.date2))))
  for (i in 1:39) print(c(i,table(is.na(bike_Share_f2[[i]]$End.date2))))

  # Checking Str 
  for (i in 1:39) print(c(i,str(bike_Share_f2[[i]]$Start.date2)))
  for (i in 1:39) print(c(1,str((bike_Share_f2[[i]]$End.date2)))) 


# Binding ---- 

rm(dc_bike1)
rm(bike_Share)
rm(bike_Share_f)


# Checking station number
for (i in 1:39) print(c(i,class(bike_Share_f2[[i]]$Start.station.number)))
for (i in 1:39) print(c(i,class(bike_Share_f2[[i]]$End.station.number)))


# join
dc_bike<-dplyr::bind_rows(bike_Share_f2)

# Checking for NAs in Dates
table(is.na(dc_bike$Start.date2))
table(is.na(dc_bike$End.date2))

# CHecking range of dates 
#range(dc_bike$Start.date2,na.rm=T)
#range(dc_bike$End.date2,na.rm=T)



# Further Formatting of Global Variables ----

  
# cleaning up whitespace in station names
  dc_bike$Start.station <- str_trim(dc_bike$Start.station,side = "both")
  dc_bike$End.station <- str_trim(dc_bike$End.station,side="both")
  
# Transforming Station numbers
  dc_bike$Start.station.number<-as.integer(dc_bike$Start.station.number)
  dc_bike$End.station.number<-as.integer(dc_bike$End.station.number)
  
  


# Export
# save(dc_bike,file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/bike_trip_unprocessed.rdata")  

  

  
# Resolving Stations: Step 1 ----
  
    
  # Cleaning up the station data from the trip data and aligning with official station data
  # Want to drop the station name/address from the trip dataset (dc_bike) and only keep the station numbers
  # The station numbers do not exist for all the names/addresses in the trip history dataset
  
  
  # 1 - Make a dc_history stations dataset
  # There are 454 unique station ids in the Trip_Stations dataset
  # When joining to locations (official data), only 450 of the station numbers are recognized.
  
  # 2 - Use this dataset to fill in records where station number is missing from history (dc_bike)
  
    
  
  
  

    # Need to resolve this into two files. One that is for trip another for stations.      
    
    # there are many records with nas for station numbers. ignore those for now
    
    
  
  # Examinining and combining start and end stations from dc_bike
  
  
  # START & END 
    Start_stations <- dc_bike %>% 
      select(Start.station,Start.station.number) %>% 
      filter(!is.na(Start.station.number)) %>% 
      unique()                                   # 570
    length(unique(Start_stations$Start.station)) # 568
    length(unique(Start_stations$Start.station.number)) # 520
          
    
    End_stations <- dc_bike %>% 
      select(End.station,End.station.number) %>% 
      filter(!is.na(End.station.number)) %>% 
      unique()                                   # 570
    length(unique(End_stations$End.station)) #568
    length(unique(End_stations$End.station.number)) # 520
    

    ## JOINIG THE TWO 
    names(Start_stations)<-c("Station_Address","Station_number")
    names(End_stations)<-c("Station_Address","Station_number")
    
    Trip_Stations<-rbind(Start_stations,End_stations) %>% unique()
    
    length(unique(Trip_Stations$Station_Address)) #568
    length(unique(Trip_Stations$Station_number)) #520
    
      
    
    
  # Custom resolution of 20 station names which have no station number 

    library(readxl)
    resolving_st<-readxl::read_excel("/home/ravi/Documents/DC BikeShare Data/DC Bike Share/Resolving Stations.xlsx",sheet = 2)
    resolving_st <- resolving_st %>% select(-`Similar to`) %>% as.data.frame()
    
    
    Trip_Stations <- rbind(Trip_Stations,resolving_st)
    
  # export  
    #save(Trip_Stations,file="trip_stations.rdata")
    #load(file="trip_stations.rdata")
    
    
    
    
  
  # Filling in the rows that have blank station numbers by using Trip_Stations
  
  LookUPVect_w_dupe <- as.vector(Trip_Stations$Station_number)
  names(LookUPVect_w_dupe)<-Trip_Stations$Station_Address
  
  
  dc_bike_edit_NAs <- dc_bike %>% filter(is.na(End.station.number)|is.na(Start.station.number))
  dc_bike_full <- dc_bike %>% filter(!(is.na(End.station.number)&is.na(Start.station.number)))
  
  #rm(dc_bike)
  
  dim(dc_bike_edit_NAs) # 9,007,793
  
  
  x1 <- dc_bike_edit_NAs[1:3000000, ]

  x2 <- dc_bike_edit_NAs[3000001:6000000, ]

  x3 <- dc_bike_edit_NAs[6000001:nrow(dc_bike_edit_NAs), ]
  
  # Replacing blank station numbers from lookup table  
  
  start<-proc.time()
  
  x1$Start.station.number <- ifelse(is.na(x1$Start.station.number),LookUPVect_w_dupe[x1$Start.station],
                                    x1$Start.station.number)
  
  x1$End.station.number <- ifelse(is.na(x1$End.station.number),LookUPVect_w_dupe[x1$End.station],
                                  x1$End.station.number)
  
  
  x2$Start.station.number <- ifelse(is.na(x2$Start.station.number),LookUPVect_w_dupe[x2$Start.station],
                                    x2$Start.station.number)
  
  x2$End.station.number <- ifelse(is.na(x2$End.station.number),LookUPVect_w_dupe[x2$End.station],
                                  x2$End.station.number)
  
  
  x3$Start.station.number <- ifelse(is.na(x3$Start.station.number),LookUPVect_w_dupe[x3$Start.station],
                                   x3$Start.station.number)
  
  x3$End.station.number <- ifelse(is.na(x3$End.station.number),LookUPVect_w_dupe[x3$End.station],
                                 x3$End.station.number)
  
  proc.time()-start  
  

  
  # Re-combining --  dc_bike_edit_clea_v1 is done with lookupVect, v2 is done with lookupVect_w_dupe
  # lookupVect_w_dupe works better - only 2% of dc_bike_edit_NAs is returning NAs
  dc_bike_edit_clean_v2<-rbind(x1,x2,x3)
  rm(x1,x2,x3)
  
  
  # HOW MANY MISSING STATION NUMBERS WERE FILLED IN?
  
  # BEFORE
  table(is.na(dc_bike_edit_NAs$Start.station.number))
  table(is.na(dc_bike_edit_NAs$End.station.number))
  
  # AFTER
  table(is.na(dc_bike_edit_clean_v2$Start.station.number))
  table(is.na(dc_bike_edit_clean_v2$End.station.number))
  
  
  

# re attaching to non_na dc_bike data
  
  dc_bike_2 <- rbind(dc_bike_edit_clean_v2,dc_bike_full)
  rm(dc_bike_edit_clean_v2,dc_bike_full,dc_bike_edit_NAs)
  
  dc_bike_2 <- dc_bike_2 %>% 
    filter(!is.na(End.station.number)) %>% 
    arrange(Start.date2) %>%
    select(-Start.station,-End.station)
  
  rm(dc_bike)
  

### Extra Station Data ----  
  ## Reading in locations
  
  bike_locations<-read.csv(file = "/home/ravi/Documents/DC BikeShare Data/DC Bike Share/Capital_Bike_Share_Locations.csv", stringsAsFactors = F,header = T)
  
  bike_locations <- bike_locations %>%
    select(ADDRESS,TERMINAL_NUMBER,LATITUDE,LONGITUDE,NUMBER_OF_BIKES,NUMBER_OF_EMPTY_DOCKS) %>%
    mutate(CAPACITY=NUMBER_OF_EMPTY_DOCKS+NUMBER_OF_BIKES) %>%
    select(-NUMBER_OF_EMPTY_DOCKS,-NUMBER_OF_BIKES)
   
  
  
  # Joining ----
  
  Stations_join <- left_join(Trip_Stations,bike_locations,
                             by=c("Station_number"="TERMINAL_NUMBER"))
  
  table(Stations_join$Station_Address==Stations_join$ADDRESS)
  # address and station name are matching on ~90% of records. Examining those that do not..
  
  Stations_join %>% filter(!Station_Address==ADDRESS) %>% 
    select(Station_Address,ADDRESS) %>% View()
  
  # these look close enough. Will adopt the ADDRESS from official file. More descriptive
  
  Stations_Final <- Stations_join %>% 
    select(-Station_Address) %>%
    filter(!is.na(Station_number))
  
  
  save(Stations_Final, file="Stations_data.rdata")  

  ### Checking Station numbers between history and stations_data
  
  hist_stations<-dc_bike_2 %>% select(Start.station.number) %>% unique()
  table(hist_stations$Start.station.number%in%Stations_Final$Station_number)
    # all are present
  

  
  
  ### Adding Final Variables ----
  
  
  # RoundTrips (4% of total trips)
  
  dc_bike_3 <- dc_bike_2
  
  #rm(dc_bike_2)
  
  
  dc_bike_3 <- dc_bike_3 %>% 
    mutate(round_trip=ifelse(Start.station.number==End.station.number,1,0))
  
  # Year
  dc_bike_3 <- dc_bike_3 %>% mutate(Start_YR=lubridate::year(Start.date2))
  
  # WeekDay (Sunday = 1)
  dc_bike_3 <- dc_bike_3 %>% mutate(Start_Weekday=lubridate::wday(Start.date2))
  
  
  
  
  ## Checking the duration times by year -- it looks like 2015, 2106 were read in 
  
  dc_bike_3 %>% 
    group_by(Start_YR) %>%
    summarise(min_dur=min(Duration),
              mean_dur=mean(Duration),
              max_dur=max(Duration))
  
  dc_bike_3 %>% 
    filter(Duration==0) %>% 
    group_by(Start_YR) %>%
    tally()

  
  
# Exporting
  
  bike_trip_processed <- dc_bike_3
  
  save(bike_trip_processed, 
       file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/bike_trip_processed.rdata")
  
  rm(dc_bike_3,dc_bike_3_post15,dc_bike_3_pre15)
  
  
  
  
  # Aggregated datasets ----
  
  load(file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/bike_trip_processed.rdata")
  load(file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/Stations_data.rdata")
  
  
  # Trips between stations
  station_to_station <- bike_trip_processed %>%
    group_by(Start.station.number,End.station.number,Start_YR,Start_Weekday,Member.type) %>%
    summarise(Total_trips=n())
  
  # Bike-specific 
  ind_bike_stats <- bike_trip_processed %>%
    group_by(Bike.number,Start_YR) %>%
    summarise(Total_trips=n(),
              Total_duration=sum(Duration))
    
  # Daily trips
  daily_Trips <- bike_trip_processed %>%
    group_by(Start.date2) %>% tally()
  
  save(ind_bike_stats,file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/ind_bike_stats.rdata")
  save(station_to_station,file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/station_to_station.rdata")
  save(daily_Trips,file="/home/ravi/Documents/DC BikeShare Data/DC Bike Share/daily_stats.rdata")

  
  