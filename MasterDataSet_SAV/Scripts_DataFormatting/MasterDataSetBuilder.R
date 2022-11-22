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

#import and merge the example csv files
#https://statisticsglobe.com/merge-csv-files-in-r
data_all <- list.files(path = "./Data_Formatted/",     # Identify all csv files in folder
                       pattern = "*flatfile.csv", full.names = TRUE) %>% 
  # Store all files in list
  lapply(read_csv) %>% 
  # Combine data sets into one data set
  bind_rows %>%          
  glimpse()


#reorder columns
#NEED TO WORK ON THIS
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



