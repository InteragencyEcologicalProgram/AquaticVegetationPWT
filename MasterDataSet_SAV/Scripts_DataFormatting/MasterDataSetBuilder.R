#Aquatic Vegetation Project Work Team
#Integrated data set
#Submersed aquatic vegetation
#Script that combines all formatted data sets 

#datasets included so far:
#DSRS
#FRANKS
#BASS
#CSTARS

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#to do list

#required packages
library(tidyverse)
library(sf)
library(deltamapr)

#import and merge the csv files
#https://statisticsglobe.com/merge-csv-files-in-r
data_all <- list.files(path = "./Data_Formatted/",     # Identify all csv files in folder
                       pattern = "*flatfile.csv", full.names = TRUE) %>% 
  # Store all files in list
  lapply(read_csv) %>% 
  # Combine data sets into one data set
  bind_rows %>%          
  glimpse()


#reorder columns
data_all_ord <- data_all %>% 
  select(program
         ,sample_method
         ,site
         ,station
         ,sample_id
         ,latitude_wgs84
         ,longitude_wgs84
         ,sample_date
         #,sample_time_pdt
         #,water_depth_m
         ,sav_incidence
         #can put this in species level table
         #repeats value for whole sample for every spp in sample
         #eventually create a sample level table to add this
         #for now, can get this value by adding up mass of spp in samples
         #relevant for BASS and DSRS
         #,sav_mass_fresh_g     
         ,species_code
         ,species_incidence
         ,species_rake_cover_percent
         ,species_rake_cover_ordinal
         ,species_mass_fresh_g
         ,species_mass_fresh_estimated_g
         ,species_mass_dry_estimated_g
         ,`species_density_fresh_g_m2`
         ,`species_density_dry_estimated_g_m2`
         ) %>% 
  glimpse()

#write integrated data set file
#write_csv(data_all_ord,"./Data_Formatted/sav_integrated_dataset_2023-03-16.csv")


#Summary stats --------------------------

#date range
range(data_all_ord$sample_date)
#"2007-10-29" "2021-10-06"

#spatial range
range(data_all_ord$latitude_wgs84,na.rm = T)
#37.78125 38.34626

range(data_all_ord$longitude_wgs84,na.rm = T)
#-121.8798 -121.3147

#how many samples in data set?
samples <- unique(data_all_ord$sample_id) 
#6,572 samples from four surveys

#which species?
species <- data_all_ord %>% 
  distinct(species_code) %>% 
  arrange(species_code) %>% 
  pull(species_code)
#19 species

#look at names of sites
sites <- data_all_ord %>% 
  distinct(program, site)

#look at station names
stations <- data_all_ord %>% 
  distinct(program, station)

#number of samples by program
prog_samp <- data_all_ord %>% 
  distinct(program, sample_id) %>% 
  group_by(program) %>% 
  count()

#Map------------

#filter data to just the unique samples with coordinates
sample_coords <- data_all_ord %>% 
  #drop samples with no coordinates
  filter(!is.na(latitude_wgs84) & !is.na(longitude_wgs84)) %>% 
  #just retain rows with distinct lat/long
  distinct(program,latitude_wgs84,longitude_wgs84) %>% 
  #specify CRS
  st_as_sf(
    coords = c("longitude_wgs84","latitude_wgs84")
    ,crs = 4326
  ) %>% 
  #change to CRS of base map
  st_transform(geometry, crs = 4269) %>% 
  glimpse()

#look at WW_Delta base map CRS
st_crs(WW_Delta) #4269

#create bounding box for map based on samples
bbox_all <- st_bbox(sample_coords)

#make map
(map_all <- ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "lightsteelblue1", color= "black") +
  #plot sampling points
  geom_sf(data= sample_coords, fill= "firebrick", shape= 21, color="black",size= 1)+
  coord_sf(
    xlim = c(bbox_all$xmin, bbox_all$xmax),
    ylim = c(bbox_all$ymin, bbox_all$ymax)
  ) + 
  theme_bw()+
  theme(
    #attempt to eliminate white margins around plot
    plot.margin=grid::unit(c(0,0,0,0), "in")
    #remove ticks marks and labels
    ,axis.title.x=element_blank()
    ,axis.text.x=element_blank()
    ,axis.ticks.x=element_blank()
    ,axis.title.y=element_blank()
    ,axis.text.y=element_blank()
    ,axis.ticks.y=element_blank()
    #drops grid lines
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    #removes plot background including large white space around margin
    ,plot.background = element_blank()
  )
)
#ggsave(file = "./Plots/sav_integrated_sample_map_2023-03-15.png",type ="cairo-png",width=6,units="in",dpi=300)













