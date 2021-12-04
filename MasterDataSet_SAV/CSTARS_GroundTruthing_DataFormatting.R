#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#CSTARS ground truthing

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) 
library(deltamapr) #Sam's package with shapefiles for delta waterways
library(janitor) #make column names tidier
library(foreign) #read dbf files


# Read in the data----------------------------------------------
# Data set is on SharePoint site for the 
# Delta Smelt Resiliency Strategy Aquatic Weed Control Action

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
sharepoint_path_read <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/CSTARS_GroundTruthing"
  )
)  

#read in the data
#start with just the Delta point data
delta_pts<-read.dbf(file = paste0(sharepoint_path_read,"./Delta202107_fieldpoints.dbf"))
glimpse(delta_pts)

#There are a lot of data columns so just start by looking at columns focused on 
#rake species
dpts <- delta_pts %>% 
  #use janitor function to clean up column names
  clean_names() %>% 
  #rename some columns
  rename(date = gps_date
         ,time = gps_time
         ,feat = feat_name  
  ) %>% 
  #reduce to just the needed columns
  select(northing
         ,easting
         ,date
         ,time
         ,feat  
         ,rake_teeth:rake_spe10
  ) %>% 
  #in prep for converting wide-ish to longest, rename some columns
  rename(rake_spec1 = rake_speci
         ,rake_prop1 = rake_spec2
         ,rake_prop3 = rake_spec4
         ,rake_prop5 = rake_spec6
         ,rake_prop7 = rake_spec8
         ,rake_prop9 = rake_spe10
  ) %>% 
  #convert wide to long
  pivot_longer(cols="rake_spec1":"rake_prop9" #select range of columns
               , names_to = c("name","num") #specify column names
               , names_pattern = '([^0-9]+)([0-9]+)' #indicate where to split names (before and after numbers)
               , values_to = "value")  %>% 
  #now pivot back to a bit wider
  pivot_wider(names_from=name, values_from=value) %>% 
  select(-num) %>% 
  glimpse()

#look at types of features
unique(dpts$feat)
#EMR Float Point_ge Riparian SAV SAV2 Unknown

#look at unique species
unique(dpts$rake_spec)
#looks like we still have some rake coverage numbers mixed in with the species
#otherwise it looks good; a list of SAV species and some algae as expected



