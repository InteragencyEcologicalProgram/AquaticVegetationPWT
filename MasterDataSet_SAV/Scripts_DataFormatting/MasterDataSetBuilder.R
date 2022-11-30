#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Script that combines all formatted data sets into master data set

#datasets included so far:
#DSRS
#FRANKS
#BASS

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
         ,sav_mass_fresh_g     
         ,species_code
         ,species_incidence
         ,species_rake_cover_percent
         ,species_rake_cover_ordinal
         ,species_mass_fresh_g
         ,species_mass_fresh_estimated_g
         ,species_mass_dry_estimated_g
         ,`species_density_fresh_g_m^2`
         ,`species_density_dry_estimated_g_m^2`
         ) %>% 
  glimpse()

#write integrated data set file
#write_csv(data_all_ord,"./Data_Formatted/sav_integrated_dataset.csv")


#Summary stats --------------------------

#how many samples in data set?
samples <- unique(data_all_ord$sample_id) #6,572 samples from four surveys




