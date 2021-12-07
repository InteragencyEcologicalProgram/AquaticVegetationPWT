#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#CSTARS ground truthing

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #work with GPS coordinates
#library(deltamapr) #Sam's package with shapefiles for delta waterways
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
  #drop the unneeded number column
  select(-num) %>% 
  #change CRS of sample coordinates
  #specify the CRS of original coordinate: presumably UTM NAD 83 (Zone 10N) (EPSG = 26910)
  st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
  #then transform to WGS84
  st_transform(4236) %>% 
  #then convert from geometry to columns
  mutate(latitude_wgs84 = unlist(map(geometry,2)),
         longitude_wgs84 = unlist(map(geometry,1))) %>% 
  #drop the geometry column
  st_set_geometry(NULL) %>% 
  glimpse()

#looked at types of features
unique(dpts$feat)
#EMR Float Point_ge Riparian SAV SAV2 Unknown

#look at number of samples per feature type
feat_count<-dpts %>% 
  distinct(date,time,feat) %>% 
  group_by(feat) %>% 
  summarize(count = n())
#SAV is most abundant category, followed by EMR, float, Riparian
#other categories are rare: Point_ge SAV2 Unknown

#before dropping them, look at non-SAV feat types
fother <- dpts %>% 
  filter(feat!="SAV") 
unique(fother$rake_spec) #all NA which makes sense
  
#look at unique species
unique(dpts$rake_spec)
#looks like we still have some rake coverage numbers mixed in with the species
#otherwise it looks good; a list of SAV species and some algae as expected

#look at cases in which species are % instead of names
#presumably these are just data entry error
#for now, will assume these should be NA for species
#but should check with UCD
ssp_chk <- dpts %>% 
  filter(grepl('%',rake_spec)) #148 rows

#look at number of samples for each % that should have been a spp
perc_count<-ssp_chk %>% 
  group_by(rake_spec) %>% 
  summarize(count = n())
#142 of 148 of these are 0%

# Making data frame with existing strings and their replacement
tr <- data.frame(target = c("SAV-S-naiad"        
                            ,"SAV-Egeria"
                            ,"SAV-Unknown"
                            ,"SAV-Elodea"
                            ,"SAV-Coontail"
                            ,"SAV-Watermilfoil"
                            ,"SAV-Rich-pondweed"
                            ,"SAV-Algae-mats"
                            ,"SAV-Sago-pondweed"  
                            ,"SAV-CrlLf-pondweed"
                            ,"SAV-Algae"
                            ,"SAV-Am-pondweed"    
                            ,"SAV-Cabomba"       
                            ,"SAV-FnLf-pondweed"  
                            ,"SAV-Tapegrass"),
                 replacement = c("Najas_guadalupensis"        
                                 ,"Egeria_densa"
                                 ,"Unidentified"
                                 ,"Elodea_canadensis"
                                 ,"Ceratophyllum_demersum"
                                 ,"Myriophyllum_spicatum"
                                 ,"Potamogeton_richardsonii"
                                 ,"Algae"
                                 #check with UCD to see if this should just be "Stuckenia_sp"
                                 ,"Stuckenia_pectinata"  
                                 ,"Potamogeton_crispus"
                                 ,"Algae"
                                 ,"Potamogeton_nodosus"    
                                 ,"Cabomba_caroliniana" 
                                 #ask UCD what sp this is
                                 ,"Potamogeton_foliosus"  
                                 ,"Vallisneria_australis"
                                 ))

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

dpts_cleaner <- dpts %>% 
  #remove duplicate rows
  #ie, cases when 2 or more of the 5 species within sample are 0%
  filter(!duplicated(.)) %>% 
  #drop all feat types except SAV
  #already checked the others and they don't have SAV rake data (unsurprisingly)
  filter(feat=="SAV") %>% 
  #remove rows where a % is present instead of a species name
  #nearly all of these are "0%"
  filter(!grepl('%',rake_spec)) %>%  #148 rows
  #format spp names
  #clean up species names
  mutate(species = str_replace_all(rake_spec,pattern = replacements)) %>% 
  #add some columns
  add_column("program"="CSTARS"
             ,"survey_method"="rake_thatch") %>% 
  #reorder columns, only keeping the necessary ones
  select("program"
         ,"latitude_wgs84"  
         ,"longitude_wgs84"
         ,"date"
         ,"time"
         ,"survey_method"
         ,"rake_teeth"
         ,"species"
         ,"rake_prop"
         ) 
#look for cases in which rake_spec is NA but rake_teeth is not 0%
#convert time to military time and pacific standard time

#combine the rake data with other useful columns from original data set
#eg, water quality data
  
#write final data frame to csv file

# Define path on SharePoint site for output files
# I synced this folder to my OneDrive
sharepoint_path_write <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
) 
#write_csv(final,file = paste0(sharepoint_path_write,"/CSTARS_2021_formatted.csv"))

  
  
  