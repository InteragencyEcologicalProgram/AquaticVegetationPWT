#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Franks Tract long term monitoring

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# To do list--------------

#still don't have full set of coordinates for 2015 (missing 100 or 50%)
#add a column to indicate whether a sample had any SAV 
#remove duplicate rows of P. pusillus

# Survey metadata------------------

#2021 data will be collected October 6th

#sampling method (Caudill et al 2019)
#weighted, double-headed, 0.33 m wide rake,
#which was dragged for ~ 3 m along the bottom

#rake coverage scoring key
# 0 = 0%
#	1 = 1-19%
#	2 = 20-39%
#	3 = 40-59%
#	4 = 60-79%
#	5 = 80-100%

# Helpful resources----------

#working with GPX files using sf package
#https://geocompr.github.io/geocompkg/articles/gps-tracks.html


# Packages--------
library(tidyverse) #suite of data science tools
library(readxl) #read excel files
library(sf) #importing gpx file and converting to data frame
library(deltamapr) #Sam's package with shapefiles for delta waterways

# Read in the data----------------------------------------------
# Data set is on SharePoint site for the 
# Delta Smelt Resiliency Strategy Aquatic Weed Control Action

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
sharepoint_path_read <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Franks_Tract"
  )
)  

#GPS coordinates for 2014 from Excel
#data author confirmed that these are the same points used for 2016
#the 2014 points overlap with the 2015 points but still 100 points for 2015 missing
#CRS is most likely WGS84
gps14e <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2014.xlsx"), range="eGERIA!A1:C101")

#GPS coordinates for 2014 from GPX file
#after overlaying both sets of 2014 coordinates on a map below, it's clear these 
#sets of points are the same
gps14g <- st_read(paste0(sharepoint_path_read,"./FT Survey Oct 2014 oDD SITES.gpx"))
#throws a warning but looks OK
gps14g #Geodetic CRS:  WGS 84

#GPS coordinates for 2017-2020 from GPX file
#it is known that the same locations were sampled across these four years
#these is another set of coordinates for this year range in some of the excel files
#after overlaying both sets of 2017-2020 coordinates on a map below, it's clear these 
#sets of points are the same
gps17g <- st_read(paste0(sharepoint_path_read,"./Franks Points.gpx"))
#throws a warning but looks OK
gps17g #Geodetic CRS:  WGS 84

#2014
#collected 10/7/2014
#GPS coordinates in other tab (i.e., eGERIA)
d14 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2014.xlsx"), range="Sheet1!A4:I104")

#2015
#collected 10/13/2015
#GPS coordinates in other tabs (i.e., eGERIA 2014, Richardson's pw 2014)
d15 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2015.xlsx"), range="2015 data!A4:K204")

#2016
#collected 10/3/2016
#GPS coordinates in other tabs (i.e., eGERIA 2014, Richardson's pw 2014)
d16 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2016.xlsx"), range="2016!A4:K49")

#2017
#collected 10/10/2017
#GPS coordinates available and imported with SAV data but different format from those for 2014
d17 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2017.xlsx"), range="2017 Data!E4:U104")

#2018
#excel file says 10/2/2018 but Jones & Thum 2021 says 10/3/2018
#GPS coordinates available and imported with SAV data but different format from those for 2014
d18 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2018.xlsx"), range="2018 Data!E4:T104")

#2019
#says 10/2/2018 which was carried over from 2018 file
#data author said it was 10/1/2019
#GPS coordinates available and imported with SAV data but different format from those for 2014
d19 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2019.xlsx"), range="2019 Data!E4:U104")

#2020
#collected 10/6/2020
#no GPS coordinates in file
d20 <- read_excel(path=paste0(sharepoint_path_read,"./Frank Tract Survey October 2020.xlsx"), range="2020data!A4:N104")

# Format data sets--------------

#convert geometry to lat/long columns for 2014 from GPX file
#these are odd numbered sites from 1-199
#fgps14b <- gps14g %>%
#  mutate(Latitude = unlist(map(gps14b$geometry,2)),
#         Longitude = unlist(map(gps14b$geometry,1)))%>% 
#  select(Latitude,Longitude) %>% 
#  add_column("name" = seq(1,by=2,len=100)) %>% 
#  st_set_geometry(NULL)  #removes geometry
#glimpse(fgps14b)

#format GPS coordinates for 2017-2020 from GPX file
#these are in WGS84 which is the CRS we want
fgps17 <- gps17g %>%
  mutate(Latitude = unlist(map(gps17g$geometry,2)),
         Longitude = unlist(map(gps17g$geometry,1)))%>% 
  st_set_geometry(NULL) %>% 
  mutate(across(c("name"), as.numeric)) %>% 
  select(name,Latitude,Longitude)
  
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

#extract 2017 GPS coordinates to add to 2020 data
gps17e <- d17 %>% 
  select("WYPT","Easting", "Northing")

#format 2018
fd18 <- d18 %>% 
  #add the sampling date
  add_column("date" = as.Date("2018-10-02", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("American PW"="Amerian PW","Egeria" = "Egeria Rating") %>% 
  #change type for some columns from logical to numeric
  mutate(across(c("American PW"), as.numeric))
#glimpse(fd18)

#format 2019
fd19 <- d19 %>% 
  #add the sampling date
  add_column("date" = as.Date("2019-10-01", "%Y-%m-%d")) %>% 
  #rename column that differs from analogs in other df's
  rename("American PW"="Amerian PW") 
#includes column for "Total" and "P. berch" which other years don't have
#also missing "Leafy PW"
#row_bind can handle this
#glimpse(fd19)

#join 2019 GPS coordinates with 2020 SAV data
d20g <- left_join(d20,gps17e)

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

# Combine data sets-----------------

#combine 2014-2016
fd1416 <- bind_rows(fd14,fd15,fd16)

#add the 2014 GPS coordinates
#shared column has different names in the two df's
#2014 GPS coordinates are latitude and longitude (probably WGS84), but
#2019 GPS coordinates are in easting and northing (probably UTMZone 10N)
fd1416g <- left_join(fd1416,gps14e, by = c("WYPT" = "Label")) 

#look at rows with NA for GPS coordinates
#should just be half of the 2015 rows (n=100)
#sum(is.na(fd1416g$Latitude)) #100 as expected

#though numbering of sites is the same for 2014-2016 and 2017-2020,
#they are not in same locations, so distinguish names for two periods
fd1416g2 <- fd1416g %>% 
  #add column containing label for 2014-2017 points
  add_column("station_set" = "A") %>% 
  #new column that concatenates station set label and waypoint number
  unite("station",c(station_set,WYPT),sep="",remove = T)
  
#combine 2017-2020
fd1720 <- bind_rows(fd17,fd18,fd19,fd20)
#bind worked even though columns weren't all in same order across df's
#and not all columns were shared across all df's
#glimpse(fd1720)

#join with df with latitude/longitude
#shared column has different names in the two df's
fd1720g <- left_join(fd1720,fgps17, by = c("WYPT" = "name")) 

#though numbering of sites is the same for 2014-2016 and 2017-2020,
#they are not in same locations, so distinguish names for two periods
fd1720g2 <- fd1720g %>% 
  #add column containing label for 2014-2017 points
  add_column("station_set" = "B") %>% 
  #new column that concatenates station set label and waypoint number
  unite("station",c(station_set,WYPT),sep="",remove = T)

#first look at structure of each
#glimpse(fd1416g2) 
#glimpse(fd1720g2)
#looks like the former is a subset of columns of the later
#and all analogous columns have identical names

#combine data sets for all years
most <-bind_rows(fd1416g2,fd1720g2)
#glimpse(most)

#clean up the (mostly) combined data set

#create vector of species names that will be column headers for wide format df
#this will be used during conversion from wide to long
sav_col<-c("Egeria_densa","Potamogeton_crispus","Ceratophyllum_demersum","Najas_guadalupensis","Stuckenia_filiformis","Stuckenia_pectinata","Elodea_canadensis","Potamogeton_richardsonii","Potamogeton_foliosus","Potamogeton_nodosus","Nitella_sp","Potamogeton_berchtoldii","Potamogeton_pusillus","Myriophyllum_spicatum","Potamogeton_zosteriformis") 

most_cleaner <- most %>% 
  #subset to just needed columns and reorder them
  select("station"
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
         ,"Stuckenia_pectinata"="Sago"
         ,"Elodea_canadensis"="Elodea"
         ,"Potamogeton_richardsonii"="Richardson's PW"
         ,"Potamogeton_foliosus"="Leafy PW"
         ,"Potamogeton_nodosus"="American PW"
         ,"Nitella_sp"="Nitella"  
         ,"Potamogeton_berchtoldii"="P. berch"        
         ,"Potamogeton_pusillus"="P.pus"          
         ,"Myriophyllum_spicatum"="Milfoil"
         ) %>% 
  #add column for rare species only mentioned in "Other Species" column
  add_column("Potamogeton_zosteriformis" = as.numeric(NA))  %>%
  pivot_longer(all_of(sav_col), names_to = "species1", values_to = "rake_coverage") %>% 
  #Potamogeton_berchtoldii is just Potamogeton_pusillus so do find and replace
  mutate(species = str_replace_all(species1,"Potamogeton_berchtoldii","Potamogeton_pusillus")) %>% 
  #add column for survey method; will distinguish between rake and visual observations
  add_column("survey_method"="rake_weighted") %>% 
  #replace NAs with zeros for rake_coverage
  replace_na(list("rake_coverage"=0)) %>% 
  #drop old species column
  select(-species1) 

#format the "other species" column
#some or all of these taxa might have been simply observed in water rather than collected on rake
#decided to categorize these as "visual" rather than "rake-weighted" survey method
#drop the hybrid note because this 2019 sample was not confirmed via genetics

# Making data frame with existing strings and their replacement
tr <- data.frame(target = c("Nitella - 1","Leafy PW", "P. Fol","Flat Stem - 1","flatstem","Flatstem","hybrid"),
                 replacement = c("Nitella","Potamogeton_foliosus","Potamogeton_foliosus","Potamogeton_zosteriformis","Potamogeton_zosteriformis","Potamogeton_zosteriformis","Potamogeton_crispus_x_Potamogeton_pusillus"))

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

#now format the other species df
other <- most %>% 
  select("Latitude","Longitude","date","station","Other Species") %>% 
  rename("other_sp" = "Other Species") %>% 
  #add column to indicate these were visual rather than rake observations
  add_column("survey_method"="visual") %>% 
  #drop all rows with NA
  drop_na() %>%   #18 remaining
  #remove all Nitella sp rows from 2017 comments to avoid double counting (n=7)
  #a column was created in original excel sheet to integrate these already
  filter(!(other_sp=="Nitella 1" & date=="2017-10-10")) %>% 
  #clean up species names
  mutate(species1 = str_replace_all(other_sp,pattern = replacements)) %>%
  #this taxa required a second round because there was so much variation in naming
  mutate(species = str_replace_all(species1,"Nitella","Nitella_sp")) %>% 
  #remove one unneeded algae row and one unconfirmed hybrid plant row
  filter(species!="Lots of algae" & species!="Potamogeton_crispus_x_Potamogeton_pusillus") %>% 
  #drop unneeded columns
  select(-c("other_sp","species1"))
#glimpse(other)

#write data to sharepoint folder
#write_csv(other,file = paste0(sharepoint_path_read,"/FranksTract_RareTaxa.csv"))

#combine rake data with visual data
#glimpse(most_cleaner)
#glimpse(other)
all_complete <- bind_rows(most_cleaner,other)
#glimpse(all_complete)

#look at range of abundance scores
unique(all_complete$rake_coverage)
hist(all_complete$rake_coverage)
rare <- all_complete %>% 
  filter(rake_coverage < 1)
#there is one sample with score of 0.01 (Nitella)

#look at rows with missing coordinates
#should just be in 2015
coords <- all_complete %>% 
  filter(is.na(Longitude))
#look at date of these observations
unique(coords$date) #"2015-10-13" - all from 2015 as expected

#final formatting
#includes some samples from 2015 without coordinates
final <- all_complete %>% 
  #change a single case of rake_coverage_ordinal from"0.01" to "1"
  #this had been typed into original excel file as "1%"
  mutate(rake_coverage_ordinal = ifelse(rake_coverage == 0.01, 1, rake_coverage)) %>% 
  #add columns with program specific info
  add_column("program" = "Franks_Tract_Management") %>% 
  #rename some columns
  rename("latitude_wgs84" = "Latitude"
         ,"longitude_wgs84" = "Longitude") %>% 
  #reorder columns
  select("program"
         ,"station"
         ,"latitude_wgs84"
         ,"longitude_wgs84"
         ,"date"
         ,"species"
         ,"survey_method"
         ,"rake_coverage_ordinal"
  )

#see if the no SAV samples were preserved properly
#n=29 I think across full time series
#sum rake scores within samples and filter to show which sum to zero
no_sav <- final %>% 
  group_by(station, date) %>% 
  summarize(sav_tot = sum(rake_coverage_ordinal)) %>% 
  filter(sav_tot ==0) %>% 
  arrange(date,station)
#29 samples as expected

# Define path on SharePoint site for output files
# I synced this folder to my OneDrive
sharepoint_path_write <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
) 
#version with some missing coordinates for 2015
#write_csv(final,file = paste0(sharepoint_path_write,"/FranksTractManagement_2014-2020_formatted.csv"))

#create version with 2015 samples that are missing coordinates removed
final_coords_complete <- final %>% 
  filter(!is.na(longitude_wgs84))

#map coordinates to compare them------------------
#two versions of both 2014-2016 points and 2017-2020 points
#GPX and Excel

#website with EPSG codes for CRS
#https://spatialreference.org/

#Note: NAD83 and WGS84 are highly similar and perhaps indistinguishable
#this explains why transformations between them appear to do nothing
#https://www.esri.com/arcgis-blog/products/arcgis-desktop/mapping/wgs84-vs-nad83/

#look at WW_Delta base map CRS
#st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#2014 Excel data: convert coordinates data frame to sf object
#probably in WGS84 (EPSG = 4236)
#then transform coordinates to NAD83 which is CRS of base layer
ggps14e <- st_as_sf(gps14e, 
                   coords = c(x='Longitude',y='Latitude'), 
                   crs = 4236) %>%  #EPSG code for WGS84
  st_transform(crs = 4269) #transform to NAD83

#2014 GPX data: format is WGS84 (EPSG = 4236)
#transform coordinates to NAD83 which is CRS of base layer
ggps14g <- gps14g %>% 
  st_transform(crs = 4269) 

#2017 Excel file: convert coordinates data frame to sf object
#probably in UTM zone 10N (EPSG = 26910)
#then transform coordinates to NAD83 which is CRS of base layer
ggps17e <- st_as_sf(gps17e, 
                    coords = c(x='Easting',y='Northing'), 
                    crs = 26910, #EPSG code for UTM zone 10N
                    remove = F) %>% #keeps source coordinates columns which we need for plotting 
  st_transform(crs = 4269) #transform to NAD83

#2017-2020 GPX file: transform coordinates to base layer CRS
ggps17g <- gps17g %>% 
  st_transform(crs = 4269) #transform to NAD83
  
#plot bay-delta base layer with sample locations from four different files
#add legend indicating which shapes/colors are which files
(sav_map_all <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot the 2014-2016 sampling locations based on Excel data
    geom_sf(data= ggps14e, fill= "yellow", color= "black", shape= 22, size= 3.5) +
    #plot the 2014-2016 sampling locations based on GPX data
    geom_sf(data= ggps14g, fill= "red", color= "black", shape= 21, size= 3) +
    #plot the 2017-2020 sampling locations based on Excel data
    geom_sf(data= ggps17e, fill= "green", color= "black", shape= 23, size= 3) +
    #plot the 2017-2020 sampling locations based on GPX data
    geom_sf(data= ggps17g, fill= "orange", color= "black", shape= 24, size= 3) +
    #zoom in on region of delta where samples were collected
    #just eyeballed the range from google maps
    #need to figure out a better way to do that in future
    coord_sf( 
      xlim =c(-121.56, -121.64),
      ylim = c(38.07, 38.02)
    )+
    theme_bw()+
    ggtitle('Franks Tract SAV Survey')
)
#ggsave(file = "FranksTract_Sampling_Map.png",type ="cairo-png",width=5, height=8,units="in",dpi=300)
#summary
#2014-2016: perfect match between Excel and GPX file
#2017-2020: perfect match between Excel and GPX file
#locations almost never match between 2014-2016 and 2017-2020 surveys
#create simplified version with just one set from each of the two periods
#and use their numbers instead of shapes

#plot bay-delta base layer with two sets of sample locations
#add legend indicating which shapes/colors are which files
(sav_map_sub <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot the 2014-2016 sampling locations based on Excel data
    geom_sf(data= ggps14e, fill= "yellow", color= "black", shape= 22, size= 3.5, show.legend = "point") +
    #plot the 2017-2020 sampling locations based on Excel data
    geom_sf(data= ggps17e, fill= "green", color= "black", shape= 23, size= 3, show.legend = "point") +
    coord_sf( 
      xlim =c(-121.56, -121.64),
      ylim = c(38.07, 38.02)
    )+
    theme_bw()+
    ggtitle('Franks Tract SAV Survey')
)
#ggsave(file = "FranksTract_Sampling_Map2.png",type ="cairo-png",width=5, height=8,units="in",dpi=300)
#legend not working yet

#plot bay-delta base layer with 2017-2021 sample locations
#add legend indicating which shapes/colors are which files
(sav_map_rec <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot the 2014-2016 sampling locations based on Excel data
    #geom_sf(data= ggps14e, fill= "yellow", color= "black", shape= 22, size= 3.5, show.legend = "point") +
    #plot the 2017-2020 sampling locations based on Excel data
    geom_sf(data= ggps17e, fill= "black", color= "black", shape= 21, size= 2, show.legend = "point") +
    coord_sf( 
      xlim =c(-121.56, -121.64),
      ylim = c(38.07, 38.02)
    )+
    theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "in"))+
    #ggtitle('Franks Tract Submerged Aquatic Vegetation Survey')+
    theme_bw()
)
#ggsave(file = paste0(sharepoint_path_read,"./FranksTract_Sampling_Map.png")
#         ,type ="cairo-png",width=6, height=6,units="in",dpi=300)

#auxillary data sets----------------
#herbicide treatments and native/non-native species status

#create data set with fluridone treatment info
#Caudill et al 2019 (Table 1): all but 2009, 2013, 2015 treated during 2006-2017
#got remaining info by emailing Division of Boating and Waterways
treatment <- data.frame("year" = c(2014:2021)
                        ,"area_treated_acres" = c(1872,0,1040,1097,1126,0,0,0)
                        ,"control_tool" = "fluridone"
)
#NOTE: have not integrated treatment data with rest of data





