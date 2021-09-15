#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Franks Tract long term monitoring

#sampling method (Caudill et al 2019)
#weighted, double-headed, 0.33 m wide rake, which was dragged for ~ 3 m along the bottom

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#need to look closer at GPS coordinates 
#for 2017-2020, do the Easting/Northing data from Excel match the 
#Latitude/Longitude data from the GPX file?

#determine how to accurately include species that were included in notes section
#confirm that the correct latin names are used for all the species codes
#make sure we have key to ordinal rake coverage categories

# Packages
library(tidyverse) #suite of data science tools
library(readxl) #read excel files
library(sf) #importing gpx file and converting to data frame

# Read in the data----------------------------------------------
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

#GPS coordinates for 2014
#these might also be the same ones for 2016 and half the ones for 2015
#need to confirm this
gps14 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2014.xlsx"), range="eGERIA!A1:C101")

#GPS coordinates for 2017-2020
#it is known that the same locations were sampled across these four years
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

#2017
#collected 10/10/2017
#GPS coordinates available and imported with SAV data but different format from those for 2014
d17 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2017.xlsx"), range="2017 Data!E4:U104")

#2018
#collected 10/2/2018
#GPS coordinates available and imported with SAV data but different format from those for 2014
d18 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2018.xlsx"), range="2018 Data!E4:T104")

#2019
#says 10/2/2018 which was carried over from 2018 file
#GPS coordinates available and imported with SAV data but different format from those for 2014
d19 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2019.xlsx"), range="2019 Data!E4:U104")

#2020
#collected 10/6/2020
#no GPS coordinates in file
d20 <- read_excel(path=paste0(sharepoint_path,"./Frank Tract Survey October 2020.xlsx"), range="2020data!A4:N104")

# Format data sets--------------

#format GPS coordinates for 2017-2020
fgps17 <- gps17 %>%
  mutate(Latitude = unlist(map(gps17$geometry,2)),
         Longitude = unlist(map(gps17$geometry,1)))%>% 
  select(name,Latitude,Longitude) %>% 
  st_set_geometry(NULL) %>% 
  mutate(across(c("name"), as.numeric))
#glimpse(fgps17)

#format 2014
fd14 <- d14 %>% 
  #inset columns missing from this df but present in others
  #also add the sampling date
  add_column("date" = as.Date("2014-10-07", "%Y-%m-%d")
             ,"Leafy PW" = as.numeric(NA)
             , "American PW" = as.numeric(NA))%>% 
  #rename columns that differs from analogs in other df's
  rename("Southern Naiad" = "Souther Naiad"
         ,"Egeria" = "Egeria Rating")
#glimpse(fd14)  
#Note: row_bind function will figure out which columns don't match among df's
#so probably unnecessary to add the missing columns by hand (except date, of course)

#format 2015
fd15 <- d15 %>% 
  #add the sampling date
  add_column("date" = as.Date("2015-10-13", "%Y-%m-%d"))%>% 
  #rename column that differs from analogs in other df's
  rename("Southern Naiad" = "Souther Naiad"
         ,"Egeria" = "Egeria Rating")
#glimpse(fd15) 

#format 2016
fd16 <- d16 %>% 
  #add the sampling date
  add_column("date" = as.Date("2016-10-03", "%Y-%m-%d")) %>% 
  rename("Egeria" = "Egeria Rating")
#glimpse(fd16) 

#format 2017
fd17 <- d17 %>% 
  #add the sampling date
  add_column("date" = as.Date("2017-10-10", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("American PW"="Amerian PW","Egeria" = "Egeria Rating") %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("American PW"), as.numeric))
#includes column for "Nitella" while 2018-2020 don't; row_bind can handle this
#glimpse(fd17)

#format 2018
fd18 <- d18 %>% 
  #add the sampling date; excel file says 10/2/2018 but manuscript says 10/3/2018
  add_column("date" = as.Date("2018-10-02", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("American PW"="Amerian PW","Egeria" = "Egeria Rating") %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("American PW"), as.numeric))
#glimpse(fd18)

#format 2019
fd19 <- d19 %>% 
  #add the sampling date; don't have specific date
  add_column("date" = as.Date("2019", "%Y")) %>% 
  #rename column that differs from analogs in other df's
  rename("American PW"="Amerian PW") 
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
#sum(is.na(d20g$Easting)) #0
#looks good

#format 2020
fd20 <- d20g %>% 
  #add the sampling date
  add_column("date" = as.Date("2020-10-06", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("American PW"="Amerian PW") %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("CLP","American PW"), as.numeric))
#glimpse(fd20)  

# Examine GPS coordinates-------------

#join 2014 and 2017 data sets to see if they match
gps_comp <- full_join(gps14,fgps17, by = c("Label" = "name"))

gps_cp <- gps_comp %>% 
  rename(Latitude14 = Latitude.x
         ,Longitude14 = Longitude.x
         ,Latitude17 = Latitude.y
         ,Longitude17 = Longitude.y
         ) %>% 
  mutate(lat_diff = Latitude14-Latitude17
         ,lon_diff = Longitude14-Longitude17)
#despite sharing some of the same site numbers, the coordinates do not match
#between 2014 and 2017-2020

#write the formatted data as csv on SharePoint
#write_csv(gps_cp,file = paste0(sharepoint_path,"/FranksTract_CoordinatesComparison.csv"))

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
sum(is.na(fd1416g$Latitude)) #100 as expected

#combine 2017-2020
fd1720 <- bind_rows(fd17,fd18,fd19,fd20)
#bind worked even though columns weren't all in same order across df's
#and not all columns were shared across all df's
#glimpse(fd1720)

#join with df with latitude/longitude
#need to figure out if lat/long and easting/northing data match
#shared column has different names in the two df's
fd1720g <- left_join(fd1720,fgps17, by = c("WYPT" = "name")) 

#first look at structure of each
#glimpse(fd1416g) 
#glimpse(fd1720g)
#looks like the former is a subset of columns of the later
#and all analogous columns have identical names

#combine data sets for all years
most <-bind_rows(fd1416g,fd1720g)
#glimpse(most)

#format the "other species" column

# Making data frame with existing strings and their replacement
tr <- data.frame(target = c("Nitella 1","Nitella - 1","Leafy PW", "P. Fol","Flat Stem - 1","flatstem","Flatstem","hybrid"),
                 replacement = c("Nitella","Nitella","Potamogeton_foliosus","Potamogeton_foliosus","Potamogeton_zosteriformis","Potamogeton_zosteriformis","Potamogeton_zosteriformis","Potamogeton_crispus_x_Potamogeton_pusillus"))

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

#now format the other species df
other <- most %>% 
  select("date","WYPT","Other Species") %>% 
  rename("other_sp" = "Other Species") %>% 
  #drop all rows with NA
  drop_na() %>% #18 remaining
  #clean up species names
  mutate(species1 = str_replace_all(other_sp,pattern = replacements)) %>%
  #this taxa required a second round because there was so much variation in naming
  mutate(species = str_replace_all(species1,"Nitella","Nitella_sp")) %>% 
  #add column to indicate that coverage is category 1 for all
  #need to check with data author to see if this is accurate
  add_column("rake_coverage" = as.numeric(1)) %>% 
  #remove one unneeded row
  filter(species!="Lots of algae") %>% 
  #drop unnneeded columns
  select(-c("other_sp","species1"))
#glimpse(other)

#write data to sharepoint folder
#write_csv(other,file = paste0(sharepoint_path,"/FranksTract_RareTaxa.csv"))

#clean up the (mostly) combined data set

#create vector of species names that will be column headers for wide format df
#this will be used during conversion from wide to long
sav_col<-c("Egeria_densa","Potamogeton_crispus","Ceratophyllum_demersum","Najas_guadalupensis","Stuckenia_filiformis","Stuckenia_pectinatus","Elodea_canadensis","Potamogeton_richardsonii","Potamogeton_foliosus","Potamogeton_nodosus","Nitella_sp","Potamogeton_berchtoldii","Potamogeton_pusillus","Myriophyllum_spicatum") 

most_cleaner <- most %>% 
  #subset to just needed columns and reorder them
  select("WYPT"
         ,"date"
         ,"Latitude"
         ,"Longitude"
         ,"Egeria"
         ,"CLP"
         ,"Coontail"
         ,"Southern Naiad"
         ,"Threadleaf PW"
         ,"Sago"
         ,"Elodea"
         ,"Richardson's PW"
         ,"Leafy PW"
         ,"American PW"
         ,"Nitella"  
         ,"P. berch"        
         ,"P.pus"          
         ,"Milfoil"
         ) %>% 
  rename("Egeria_densa"="Egeria"
         ,"Potamogeton_crispus"="CLP"
         ,"Ceratophyllum_demersum"="Coontail"
         ,"Najas_guadalupensis"="Southern Naiad"
         ,"Stuckenia_filiformis"="Threadleaf PW"
         ,"Stuckenia_pectinatus"="Sago"
         ,"Elodea_canadensis"="Elodea"
         ,"Potamogeton_richardsonii"="Richardson's PW"
         ,"Potamogeton_foliosus"="Leafy PW"
         ,"Potamogeton_nodosus"="American PW"
         ,"Nitella_sp"="Nitella"  
         ,"Potamogeton_berchtoldii"="P. berch"        
         ,"Potamogeton_pusillus"="P.pus"          
         ,"Myriophyllum_spicatum"="Milfoil"
         ) %>% 
  pivot_longer(all_of(sav_col), names_to = "species", values_to = "rake_coverage") %>% 
  #drop all rows with NA
  drop_na("rake_coverage") 
  
#combine main df with "other species" df
#glimpse(most_cleaner)
#glimpse(other)
all_complete <- bind_rows(most_cleaner,other)

#all unique taxa
unique(all_complete$species)

#look at observations for rare taxa
berch <- all_complete %>% 
  filter(species=="Potamogeton_berchtoldii")

zost <- all_complete %>% 
  filter(species=="Potamogeton_zosteriformis")

pusl <- all_complete %>% 
  filter(species=="Potamogeton_pusillus")

#look at range of abundance scores
range(all_complete$rake_coverage) #0.01 5.00
hist(all_complete$rake_coverage)
unique(all_complete$rake_coverage)
rare <- all_complete %>% 
  filter(rake_coverage < 1)
#there is one sample with score of 0.01
#presumably trace amounts of Nitella

#add column to indicate which survey generated the data
#finalize column headers
#export this file back to sharepoint
                       

 

