#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Franks Tract long term monitoring

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#import data from sharepoint
#most data are in tabs of excel files
#some coordinates in GPX file
#missing data for some years still

# Packages
library(tidyverse) #suite of data science tools
library(readxl) #read excel files
library(sf) #importing gpx file and converting to data frame

# Read in the Data----------------------------------------------
# Data set is on SharePoint site for the 
# Delta Smelt Resiliency Strategy Aquatic Weed Control Action

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Franks_Tract"
  )
)  

#GPS coordinates for 2014-2016
#this is only half of the 2015 waypoints though
gps14 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2014.xlsx"), range="eGERIA!A1:C101")

#GPS coordinates for 2017-2020
gps17 <- st_read(paste0(sharepoint_path,"./Franks Points.gpx"))
#throws a warning but looks OK

#2014
#collected 10/7/2014
#GPS coordinates in other tab (i.e., eGERIA)
d14 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2014.xlsx"), range="Sheet1!A4:I104")

#2015
#collected 10/13/2015
#GPS coordinates in other tabs (i.e., eGERIA 2014, Richardson's pw 2014)
d15 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2015.xlsx"), range="2015 data!A4:K204")

#2016
#collected 10/3/2016
#GPS coordinates in other tabs (i.e., eGERIA 2014, Richardson's pw 2014)
d16 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2016.xlsx"), range="2016!A4:K49")

#Data for 2017-2020 are collected at the same 100 waypoints each year 
#and I have those waypoints

#2017
#collected 10/10/2017
#GPS coordinates available and imported with SAV data
d17 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2017.xlsx"), range="2017 Data!E4:U104")

#2018
#collected 10/2/2018
#GPS coordinates available and imported with SAV data
d18 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2018.xlsx"), range="2018 Data!E4:T104")

#2019
#says 10/2/2018 which was carried over from 2018 file
#GPS coordinates available and imported with SAV data
d19 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2019.xlsx"), range="2019 Data!E4:U104")

#2020
#collected 10/6/2020
#no GPS coordinates 
d20 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2020.xlsx"), range="2020data!A4:N104")

# Format data sets--------------
# they all need to have same format to be combined

#need to figure out GPS coodinates still
#either figure out how to get eastings/northings into lat/long
#or figure out how to read the gpx file properly (pretty close now)

#format GPS coordinates for 2017-2020
fgps17 <- gps17 %>% 
  select(name,geometry) %>% 
  extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) 

sep <- cbind(fgps17, st_coordinates(fgps17))


sepp <- extract(data = fgps17, col = geometry, into = c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) 

separated_coord <- fgps17  %>%
  mutate(lat = unlist(map(geometry,1)),
         long = unlist(map(geometry,2))
         )

#format 2014
fd14 <- d14 %>% 
  #inset columns missing from this df but present in others
  #also add the sampling date
  add_column("date" = as.Date("2014-10-07", "%Y-%m-%d")
             ,"Leafy PW" = as.numeric(NA)
             , "American PW" = as.numeric(NA))%>% 
  #rename column that differs from analogs in other df's
  rename("Southern Naiad" = "Souther Naiad")
#glimpse(fd14)  
#Note: row_bind function will figure out which columns don't match among df's
#so probably unnecessary to add the missing columns by hand (except date, of course)

#format 2015
fd15 <- d15 %>% 
  #add the sampling date
  add_column("date" = as.Date("2015-10-13", "%Y-%m-%d"))%>% 
  #rename column that differs from analogs in other df's
  rename("Southern Naiad" = "Souther Naiad")
#glimpse(fd15) 

#format 2016
fd16 <- d16 %>% 
  #add the sampling date
  add_column("date" = as.Date("2016-10-03", "%Y-%m-%d")) 
#glimpse(fd16) 

#format 2017
fd17 <- d17 %>% 
  #add the sampling date
  add_column("date" = as.Date("2017-10-10", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("Egeria" = "Egeria Rating") %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("Amerian PW"), as.numeric))
#includes column for "Nitella" while 2018-2020 don't; row_bind can handle this
#glimpse(fd17)

#format 2018
fd18 <- d18 %>% 
  #add the sampling date
  add_column("date" = as.Date("2018-10-02", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("Egeria" = "Egeria Rating") %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("Amerian PW"), as.numeric))
#glimpse(fd18)

#format 2019
fd19 <- d19 %>% 
  #add the sampling date; don't have specific date
  add_column("date" = as.Date("2019", "%Y"))
#includes column for "Total" and "P. berch" which other years don't have
#also missing "Leafy PW"
#row_bind can handle this
#glimpse(fd19)

#extract 2019 GPS coordinates to add to 2020 data
gps19 <- d19 %>% 
  select("WYPT","Easting", "Northing")

#join 2019 GPS coordinates with 2020 SAV data
d20g <- left_join(d20,gps19)

#see if any GPS coordinates failed to join properly
sum(is.na(d20g$Easting)) #0
#looks good

#format 2020
fd20 <- d20g %>% 
  #add the sampling date
  add_column("date" = as.Date("2020-10-06", "%Y-%m-%d")) %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("CLP","Amerian PW"), as.numeric))
#glimpse(fd20)  

# Species---------------
#P. berch: Potamogeton berchtoldii 

# Combine data sets-----------------

#combine 2014-2016
fd1416 <- bind_rows(fd14,fd15,fd16)

#add the 2014 GPS coordinates
#shared column has different names in the two df's
#2014 GPS coordinates are latitude and longitude, but
#2019 GPS coordinates are in easting and northing
fd1416g <- left_join(fd1416,gps14, by = c("WYPT" = "Label")) 

#look at rows with NA for GPS coordinates
#should just be half of the 2015 rows (n=100)
sum(is.na(fd1416g$Latitude)) #100
#as expected

#combine 2017-2020
fd1720 <- bind_rows(fd17,fd18,fd19,fd20)
#bind worked even though columns weren't all in same order across df's
#and not all columns were shared across all df's
glimpse(fd1720)

#convert eastings and northings to latitude and longitude for 2017-2020 data
  




