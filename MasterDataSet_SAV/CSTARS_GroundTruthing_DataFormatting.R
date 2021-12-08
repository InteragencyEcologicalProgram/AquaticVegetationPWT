#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#CSTARS ground truthing

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#to do list
#format the visual survey data

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #work with GPS coordinates
library(janitor) #make column names tidier
library(foreign) #read dbf files
library(hms) #formatting time

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
#glimpse(delta_pts)

#Rake spp data formatting-------------

dpts <- delta_pts %>% 
  #use janitor function to clean up column names
  clean_names() %>% 
  #rename some columns
  rename(date = gps_date
         ,time = gps_time
         ,feat = feat_name 
         #need units for depth
         ,depth_to_sav = depth_to_s
         #need units for depth; based on histogram of data, most likely feet
         ,secchi_depth = secchi_dep 
         #need to determine if I interpretted this correctly
         ,secchi_bottom = secchi_b
  ) %>% 
  #convert feet to meters for depth measurements
  mutate(secchi_depth_m = secchi_depth*0.3048
         ,depth_to_sav_m = depth_to_sav*0.3048
         ) %>% 
  #removes all % from data frame but converts all columns to character which is a hassle
  #mutate_all(str_replace_all, "%", "")
  #instead remove % from specific columns and make those columns numeric
  mutate(rake_teeth= str_replace(rake_teeth,pattern = "%", replacement = "")
         ,rake_spec2 = str_replace(rake_spec2,pattern = "%", replacement = "") 
         ,rake_spec4 = str_replace(rake_spec4,pattern = "%", replacement = "")
         ,rake_spec6 = str_replace(rake_spec6,pattern = "%", replacement = "")
         ,rake_spec8 = str_replace(rake_spec8,pattern = "%", replacement = "")
         ,rake_spe10 = str_replace(rake_spe10,pattern = "%", replacement = "")
         ,across(c(rake_teeth,rake_spec2,rake_spec4,rake_spec6, rake_spec8, rake_spe10),as.numeric)
  ) %>% 
  #create new column that calculates total rake coverage of spp
  #should all be either 0 or 100; most but not all of them are, indicating some errors
  rowwise() %>% 
  mutate(tot_cover_spp = sum(c(rake_spec2,rake_spec4,rake_spec6, rake_spec8, rake_spe10),na.rm=T)) %>% 
  #make new columns that will allow for comparison of rake coverage to spp level coverage
  #values for rake_teeth_logical and tot_cover_spp_logical should match
  #ie, both be zero or both be one; mismatch indicates errors
  mutate(rake_teeth_logical = if_else(rake_teeth > 0, 1,0)
         ,tot_cover_spp_logical = if_else(tot_cover_spp > 0, 1,0)
         ,rake_diff = rake_teeth_logical-tot_cover_spp_logical
         #replace incorrect cases of rake_teeth=100% to rake_teeth=0%
         #there remain four cases in which there is a spp listed but with no cover %
         #likely the rake_teeth column was incorrectly used instead of the spp cover column
         #probably should just delete these four because we won't know the rake_teeth value
         #could later ask UCD to reference their field photos to correct these
         ,rake_teeth_corr = if_else((rake_diff==1 & is.na(rake_speci)),0,rake_teeth)
         ) %>% 
  #drop three rows where ssp cover % was incorrectly put into rake_teeth column
  filter(!(rake_diff==1 & !is.na(rake_speci))) %>% 
  #reduce to just the needed columns
  select(northing
         ,easting
         ,date
         ,time
         ,feat  
         ,depth_to_sav_m
         ,rake_teeth
         ,rake_teeth_corr
         ,rake_speci:rake_spe10
         ,secchi_depth_m
         ,secchi_bottom
         ,treated
         ,tot_cover_spp:rake_diff
         ,comments
  ) %>% 
  glimpse()

#look for cases in which values for rake_teeth_logical and tot_cover_spp_logical don't match
dpts_check <- dpts %>% 
  filter(rake_diff!=0)
#no cases of rake_teeth being zero and spp cover being non-zero
#all are cases in which rake_teeth is non-zero while there is no spp specific cover data
#nearly all are rake_teeth = 100% with no spp specific cover
#presumably these should just be changed to rake_teeth=0%; nothing helpful in comments
#in three cases 0<rake_teeth<100 and there is a spp named in rake_spec1 column
#likely indicating that data were just entered into the wrong column

#histogram of total rake coverage
hist(dpts$tot_cover_spp)
cover_count<-dpts %>%
  group_by(tot_cover_spp) %>%
  summarize(count = n())
#most are either zero or 100 but there are various others, which indicate errors

#convert data frame from wide to long
dpts_long <- dpts %>% 
  #in prep for converting wide-ish to longest, rename some columns
  rename(rake_spec1 = rake_speci
         ,rake_prop1 = rake_spec2
         ,rake_prop3 = rake_spec4
         ,rake_prop5 = rake_spec6
         ,rake_prop7 = rake_spec8
         ,rake_prop9 = rake_spe10
  )  %>% 
  #convert the rake prop columns back to factors for converting from wide to long
  mutate(across(c(rake_prop1,rake_prop3,rake_prop5,rake_prop7,rake_prop9),as.factor)) %>% 
  #convert wide to long
  pivot_longer(cols="rake_spec1":"rake_prop9" #select range of columns
               , names_to = c("name","num") #specify column names
               , names_pattern = '([^0-9]+)([0-9]+)' #indicate where to split names (before and after numbers)
               , values_to = "value")  %>% 
  #now pivot back to a bit wider
  pivot_wider(names_from=name, values_from=value) %>% 
  #drop unneeded columns: ssp num, original rake_teeth with errors,
  select(-c(num,rake_teeth,rake_teeth_logical:comments)) %>% 
  glimpse()

#looked at types of features
unique(dpts_long$feat)
#EMR Float Point_ge Riparian SAV SAV2 Unknown

#look at number of samples per feature type
feat_count<-dpts_long %>% 
  distinct(date,time,feat) %>% 
  group_by(feat) %>% 
  summarize(count = n())
#SAV is most abundant category, followed by EMR, float, Riparian
#other categories are rare: Point_ge SAV2 Unknown

#before dropping them, look at non-SAV feat types
fother <- dpts_long %>% 
  filter(feat!="SAV") 
unique(fother$rake_spec) #all NA which makes sense
  
#look at unique species
unique(dpts_long$rake_spec)
#looks like we still have some rake coverage numbers mixed in with the species
#otherwise it looks good; a list of SAV species and some algae as expected

#look at cases in which species are % instead of names
#presumably these are just data entry error
#for now, will assume these should be NA for species
#but should check with UCD
ssp_chk <- dpts_long %>% 
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
                            #ask UCD what sp this is
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
                                 #I guessed on this species
                                 ,"Potamogeton_foliosus"  
                                 ,"Vallisneria_australis"
                                 ))

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

#continue formatting data set
dpts_cleaner <- dpts_long %>% 
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
  mutate(across(c(time),as.character)) %>% 
  #reorder columns, only keeping the necessary ones
  select("program"
         ,"latitude_wgs84"  
         ,"longitude_wgs84"
         ,"date"
         ,"time"
         ,"depth_to_sav_m"
         ,"survey_method"
         ,"rake_teeth_corr"
         ,"species"
         ,"rake_prop"
         ,"secchi_depth_m"    
         ,"secchi_bottom"   
         ,"treated"
         ) 
#convert time to military time and pacific standard time
#consider changing NA for species to None
#need better explanation of comments
  #I think sometimes it is indicating trace amounts of a species
  #and sometimes identifying a rare species not in the drop down menu
  #so the comment might be IDing "SAV-Unknown"

#format date to military version of Pacific Standard Time
#dpts_cleaner$time_pst <-as_hms(dpts_cleaner$time) 

#look at depth to sav
hist(dpts_cleaner$depth_to_sav_m)
d_sav_count<-dpts_cleaner %>% 
  group_by(depth_to_sav_m) %>% 
  summarize(count = n())

#look at secchi depth
hist(dpts_cleaner$secchi_depth_m)
secchi_count<-dpts_cleaner %>% 
  filter(secchi_depth_m>=0) %>% 
  group_by(secchi_depth_m) %>% 
  summarize(count = n())
#a lot of -99 values; presumably means not measured but should check with UCD
sec<-dpts_cleaner %>% 
  filter(secchi_depth_m>=0) 
hist(sec$secchi_depth_m)

#look at secchi to bottom
unique(dpts_cleaner$secchi_bottom)
bottom_count<-dpts_cleaner %>% 
  group_by(secchi_bottom) %>% 
  summarize(count = n())
#vast majority are "no" which makes sense because they sample in shallow areas

#look at treated
#ask UCD what this is based on
unique(dpts_cleaner$treated)
treat_count<-dpts_cleaner %>% 
  group_by(treated) %>% 
  summarize(count = n())
#all are "no"

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

  
  
  