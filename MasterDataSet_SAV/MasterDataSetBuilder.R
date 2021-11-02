#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Script that combines all formatted data sets into master data set

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#to do list
#still need to tweak column names, format, and order to maximize consistency among data sets

#required packages
library(tidyverse)

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
dir_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
)  

#import and merge the example csv files
#https://statisticsglobe.com/merge-csv-files-in-r
data_all <- list.files(path = dir_path,     # Identify all csv files in folder
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set 



#reorder columns
data_all_ord <- data_all %>% 
  select("program"
         ,"station"
         #,"id"
       , "date"
       , "latitude_wgs84"
       , "longitude_wgs84"
       , "species"
       , "survey_method"
       #, "incidence"
       ,"rake_coverage_ordinal"
       , "biomass_fresh_g"
       , "biomass_dry_g"
       ,"density_fresh_g_m^2"
       ,"density_dry_g_m^2"
       , "water_depth_m"
) %>% 
  glimpse()



