#Drought Barrier Report
#Franks Tract Study
#Kriging maps of SAV abundance
#do one for each year 2014-2021

#helpful online reference
#https://swilke-geoscience.net/post/spatial_interpolation/

#packages
# We will need some packages for (spatial) data processing
library(tidyverse) # wrangling tabular data and plotting
library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!

# And a lot of different packages to test their interpolation functions
library(gstat)  # inverse distance weighted, Kriging
library(fields) # Thin Plate Spline
library(interp) # Triangulation
library(mgcv)   # Spatial GAM
library(automap)# Automatic approach to Kriging

# Finally, some packages to make pretty plots
library(patchwork)
library(viridis)

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
dir_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
)  

#read in the data
cleandat <- read_csv(file=paste0(dir_path,"./FranksTractManagement_2014-2020_formatted.csv"))

#create data set with native vs non-native status of all species
spp <- unique(cleandat$species)
native <- data.frame("species" = c(spp)
                     ,"native" = c("n","n",rep("y",10),"n","y")
                     )
#write_csv(native,file = paste0(dir_path,"/FranksTractManagement_SpeciesOrigin.csv"))


#create data set with fluridone treatment info
#Caudill et al 2019 (Table 1): all but 2009, 2013, 2015 treated during 2006-2017
#got remaining info by emailing Division of Boating and Waterways
treatment <- data.frame("year" = c(2014:2021)
                        ,"area_treated_acres" = c(1872,0,1040,1097,1126,0,0,0)
                        ,"control_tool" = "fluridone"
)
#NOTE: have not integrated treatment data with rest of data
#write_csv(treatment,file = paste0(dir_path,"/FranksTractManagement_HerbicideTreatments.csv"))



#sum scores by sample
sumdat <- cleandat %>% 
  group_by(date,latitude_wgs84,longitude_wgs84) %>% 
  summarize(sav_tot = sum(rake_coverage_ordinal)) %>% 
  #drop all records without GPS coordinates
  filter(!is.na(longitude_wgs84)) %>% 
  #filter(sav_tot ==0) %>% #23 are zeros which sounds about right
  glimpse()

#plot histogram of sum scores
hist(sumdat$sav_tot)
#ranges up to 15 which seems a bit high

#mean scores by sample
avgdat <- cleandat %>% 
  group_by(date,latitude_wgs84,longitude_wgs84) %>% 
  #drop all rows with zeros
  #probably reasonable to do for rare species in samples with some veg
  #what about samples with no veg at all? should probably keep those
  summarize(sav_tot = mean(rake_coverage_ordinal)) %>% 
  #drop all records without GPS coordinates
  filter(!is.na(longitude_wgs84)) %>% 
  #filter(sav_tot ==0) %>% #23 are zeros which sounds about right
  glimpse()

#plot histogram of mean scores
hist(avgdat$sav_tot)
#ranges up to 15 which seems a bit high

#how many samples over 10
high <- sumdat %>% 
  filter(sav_tot>7)
#n=15

#look closer at sample with score of 15
vhigh <- cleandat %>% 
  filter(station == "B90" & date == "2020-10-06")
#matches original data sheet

#start by plotting just the 2020 data set
m20 <- sumdat  %>% 
  filter(date > "2019-12-31")
#side note: there are several missing values; need to figure out why

#initial plot of points color coded by score
#all points for all years (ignoring date for now)
(point_plot <- ggplot(
  data = m20,
  mapping = aes(x = latitude_wgs84, y = longitude_wgs84, color = sav_tot)) +
  geom_point(size = 3) +
  scale_color_gradientn(colors = c("blue", "yellow", "red"))
)


#create a grid template
bbox <- c(
  "xmin" = min(-121.56),
  "ymin" = min(-121.64),
  "xmax" = max(38.07),
  "ymax" = max(38.02)
)

grd_template <- expand.grid(
  X = seq(from = bbox["xmin"], to = bbox["xmax"], by = 20),
  Y = seq(from = bbox["ymin"], to = bbox["ymax"], by = 20) # 20 m resolution
)

(grid_plot <- ggplot() +
  geom_point(data = grd_template, aes(x = X, y = Y), size = 0.01) +
  geom_point(data = sumdat,
             mapping = aes(x = latitude_wgs84, y = longitude_wgs84, color = sav_tot), size = 3) +
  scale_color_gradientn(colors = c("blue", "yellow", "red")) +
  #coord_cartesian( #zooming in so we can actually see something
   # xlim = c(408000, 409000), ylim = c(5815000, 5816000)) +
  theme_bw()
)
#NOTE: this isn't working. possibly because we need to specify the coordinate system or something





































