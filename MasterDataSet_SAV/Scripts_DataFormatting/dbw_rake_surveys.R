#Aquatic Vegetation Project Work Team
#Integrated data set
#Submersed aquatic vegetation
#Division of Boating and Waterways rake survey data

#load packages
library(tidyverse) #data science tools
library(sf) #tools for spatial analysis, including reading in shp and dbf files
library(readxl) #read excel files
library(deltamapr) #Sam's delta shapefiles
library(janitor) #clean up column header names

#Notes
#for 2017-2019 and part of 2020, the waypoint names and GPS coordinates are in .shp files and veg data are in .xlsx files
#connect the two file types by waypoint ID
#probably missing the 2020 data that were collected using Survey123; should ask Trish for these
#for 2021, all data should be in GIS files, need to look closer at these

#read in all the xlsx files with the veg data-------
#note this could be really annoying because of the way the data are formatted

#2017 files
#not sure the differences among them

#note there are two columns for each species; probably min and max score?
dbw17x <- read_excel(path="./Data_Raw/dbw/2017-PointSampling-12weekSites-filled.xlsx",skip = 15) %>%
  clean_names() %>% 
  glimpse()

#pre and post rake data in different tabs
#first sheet is a mess because there are 15 rows on each page of each spreadsheet that need to be skipped
#second sheet looks OK (ie, post), so start with that
dbw17post <- read_excel(path="./Data_Raw/dbw/2017-PointSamplingData Sheet2.xlsx",sheet = "2017 Post",skip = 15) %>%
  clean_names() %>% 
  glimpse()
#mostly worked; there's a date in one cell where there should be an integer; I think it should be "1" and "4" in the 
#two columns for fanwort

dbw17x <- read_excel(path="./Data_Raw/dbw/2017-PointSampling-12weekSites-filled.xlsx",skip = 15) %>%
  clean_names() %>% 
  glimpse()


#read in shp files-------------

#Create character vectors of 2017-2020 shp files (n=4)
#read in 2021 files separately
dbw_geo <- dir(path = "./Data_Raw/dbw", pattern = ".shp", full.names = T,recursive = F)

#would be nice to just combine all these shp files into one dataframe
#but the columns aren't consistent among files
#also each of these shp files will have to be combined separately with its respective xlsx file
#each of which is also formatted differently

#2017
dbw17g <- read_sf(dbw_geo[4]) %>% 
  clean_names() %>% 
  glimpse()

#2018  
dbw18g <- read_sf(dbw_geo[1])%>% 
  clean_names() %>% 
  glimpse()

#2019
dbw19g <- read_sf(dbw_geo[3])%>% 
  clean_names() %>% 
  glimpse()

#2020
dbw20g <- read_sf(dbw_geo[2])%>% 
  clean_names() %>% 
  glimpse()





#map the coordinates for each year to see if they look correct----------
#maps for all four years look good

#check the CRS for each shp file
#all four are NAD83 / UTM zone 10N (EPSG = 26910)
st_crs(dbw17g)
st_crs(dbw18g)
st_crs(dbw19g)
st_crs(dbw20g)  

#look at CRS of delta base map
st_crs(WW_Delta) #EPSG = 4269

#change CRS of base map to that of rake points
WW_Delta_f <- WW_Delta %>% 
  st_transform(crs = 26910)

#create bounding box for each annual map
bbox_17 <- st_bbox(dbw17g)
bbox_18 <- st_bbox(dbw18g)
bbox_19 <- st_bbox(dbw19g)
bbox_20 <- st_bbox(dbw20g)

#plot 2017 data points 
(p17 <- ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta_f, fill= "lightsteelblue1", color= "black") +
  #plot SAV sampling points
  geom_sf(data= dbw17g, fill= "red", shape= 22, color="black",size= 2)+
  coord_sf(
    xlim = c(bbox_17$xmin, bbox_17$xmax),
    ylim = c(bbox_17$ymin, bbox_17$ymax)
  ) + 
    ggtitle("2017")+
  theme_bw()
)

#plot 2018 data points 
(p18 <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_f, fill= "lightsteelblue1", color= "black") +
    #plot SAV sampling points
    geom_sf(data= dbw18g, fill= "blue", shape= 22, color="black",size= 2)+
    coord_sf(
      xlim = c(bbox_18$xmin, bbox_18$xmax),
      ylim = c(bbox_18$ymin, bbox_18$ymax)
    ) + 
    ggtitle("2018")+
    theme_bw()
)

#plot 2019 data points 
(p19 <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_f, fill= "lightsteelblue1", color= "black") +
    #plot SAV sampling points
    geom_sf(data= dbw19g, fill= "green", shape= 22, color="black",size= 2)+
    coord_sf(
      xlim = c(bbox_19$xmin, bbox_19$xmax),
      ylim = c(bbox_19$ymin, bbox_19$ymax)
    ) + 
    ggtitle("2019")+
    theme_bw()
)

#plot 2020 data points 
(p20 <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_f, fill= "lightsteelblue1", color= "black") +
    #plot SAV sampling points
    geom_sf(data= dbw20g, fill= "orange", shape= 22, color="black",size= 2)+
    coord_sf(
      xlim = c(bbox_20$xmin, bbox_20$xmax),
      ylim = c(bbox_20$ymin, bbox_20$ymax)
    ) + 
    ggtitle("2020")+
    theme_bw()
)

#match up the year specific info for waypoint and GPS coordinates with year specific veg data--------










