#Aquatic Vegetation Project Work Team
#Integrated data set
#Submersed aquatic vegetation
#CSTARS ground truthing
#includes all Delta points 
#does not include Suisun Marsh points
#raw data pulled from EDI

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#Nick to do list-------

#decide whether to keep either of these columns
#Location is where the boat was in relation to the patch - "center" meaning - on the patch
#"alongside" meaning - right next to the patch; 
#and "range-finder" meaning - center of patch but by use of distance-offset 
#where distance is using the range-finder, and angle is using the compass - hence chance of error is more.
#PositionIn meaning "position within the patch" - "center" meaning 
#only 1 point per patch somewhere within the patch while "edge" means more likely 
#two points were taken at the ends of a long patch.

#consider adding the program as a prefix to station to make sure it is unique
#when combined with other data sets

#need to confirm time zone (PDT vs PST), though currently we don't include time in integrated data set

#consider changing sago species to just genus


#CSTARS to do list-------

#add the open water samples to this data set
#for now, note in the EDI metadata that these are missing

#fix duplicate for station 4148
#for now, I made them 4148.1 and 4148.2

#fix the Fall 2019 samples with missing dates
#for now, I inputed the middle date for the period (10/4/19) 

#one case of rake teeth = -1.11 which must be an error
#for now, I just made it positive 1.11

#fix the samples where it is indicated that SAV was present on the rake
#but there's no corresponding spp level data
#for now, I will drop these from the data set

#fix samples in which spp composition number don't add roughly to either 
#zero or one
#for now, I will leave them in as is

#for station 1081, depth to sav has a bunch of junk in it
#for now, I just made this cell NA


# Packages--------
library(tidyverse) #suite of data science tools
library(janitor) #make column names tidier
library(lubridate) #formatting dates
library(sf) #work with GPS coordinates
library(deltamapr) #maps of the delta

#read in data from EDI-----------

#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1143

sav_rake_data <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1143.1&entityid=c818f952f9401a32678484fea246dc5a") %>% 
  #automatically clean column name format
  clean_names()
#glimpse(sav_rake_data)

#read in taxonomy data set to get species codes
taxonomy <- read_csv("./Data_Formatted/taxonomy_all.csv") %>% 
  select(species,species_code)

#initial data exploration------------

#look at date range
#range(sav_rake_data$date,na.rm=T)
#"2007-10-29" "2021-09-13"
#NOTE: there are some NAs for date

#look at rows with date = NA
#d_miss <- sav_rake_data %>% 
#  filter(is.na(date))  
#13 samples from Sept 2019 missing date
#note there is content in the yyyymm column for these samples
#so if you delete this latter column there is no longer any indication
#of when the sample was collected

#look at range of 2019 dates so we can consider a reasonable date to inpute for NAs
#sav19 <- sav_rake_data %>% 
  #add year column
#  mutate(year=as.factor(str_sub(yyyymm,1,4))) %>% 
#  filter(year==2019 & date > "2019-06-01") %>% 
#  distinct(date)
#fall 2019 dates are all within 10/1-10/10
#we'll use 10/4 because it's the middle date

#are all stations/field IDs unique
#sav_stations <-unique(sav_rake_data$orig_fid)
#looks like they are unique, with one exception

#station_check <- filter(sav_rake_data, (duplicated(orig_fid))) #one duplicate for 4148

#look at duplicates
#station_dup <- filter(sav_rake_data, orig_fid==4148)
#this issue is fixed in code below


#fix issues in columns used by both rake and patch data tables----------
#a duplicated station number
#missing dates
#a sample with negative rake abundance (-1.11)

sav_rake <- sav_rake_data %>%
  mutate(
    #inpute dates for the missing Fall 2019 dates
    date2 = case_when(is.na(date) ~ ymd("2019-10-04"),TRUE ~ date)
    #deal with duplicate station names
    ,station = case_when(orig_fid==4148 & pat_len=="M (5-10m)" ~ 4148.1
                         ,orig_fid==4148 & pat_len=="XL (>15m)"~ 4148.2
                         , TRUE ~ orig_fid)
    #add program prefix to sample number
    ,sample_id = str_c("CSTARS_",station)
    #to get rid of negative rake value, just use absolute value of all numbers
    ,rake_teeth = abs(rk_tth)
  )
  
#look at rows with date = NA again
#d_miss2 <- sav_rake %>% 
#  filter(is.na(date2))
#dates are fixed

#look at station duplicates again
#station_dup2 <- filter(sav_rake, orig_fid==4148)
#change worked

#is the negative rake value fixed
#hist(sav_rake_data$rk_tth)
#hist(sav_rake$rake_teeth)
#yes it's fixed

#create taxonomy table-------------
#include six letter spp codes and latin names

#taxonomy <- as_tibble(
#  cbind(
#    "code" = c("EGEDEN","CERDEM","MYRSPI","POTNOD","POTCRI","STUPEC","CABCAR","ELOCAN","POTRIC"
#               ,"NAJGUA","POTPUS","ECHBER","VALAUS","HETDUB","FILALG")
#    ,"species" = c("Egeria_densa","Ceratophyllum_demersum","Myriophyllum_spicatum"
#                   ,"Potamogeton_nodosus","Potamogeton_crispus","Stuckenia_pectinata"
#                   ,"Cabomba_caroliniana","Elodea_canadensis","Potamogeton_richardsonii"
#                   ,"Najas_guadalupensis","Potamogeton_pusillus", "Echinodorus_berteroi"
#                   ,"Vallisneria_australis","Heteranthera_dubia","filamentous_algae")
#    ,"native" = c("n","n","n","y","n","y","n","y","y","y","y","y","n","y","y")
#  )
#  )

#taxonomy_final <- taxonomy %>% 
#  separate(species, into = (c("genus","specific_epithet")),sep ="_",remove=F) %>% 
#  arrange(species) %>% 
#  select(code,genus,specific_epithet,species,native)

#write_csv(taxonomy_final,"Data_Formatted/cstars_taxonomy.csv")


#format rake level data -----------------

rake_format <- sav_rake %>% 
  #only keep spp level columns and rename them as needed
  select(sample_id 
         ,sample_date = date2
         ,sample_time_pdt = time #still need to confirm time zone
         ,latitude_wgs84 = latitude
         ,longitude_wgs84 = longitude
         ,rake_teeth
         ,EGEDEN = egeria
         ,CERDEM = coontail     
         ,MYRSPI = watermilfoil 
         ,FILALG= algae        
         ,POTNOD = am_pondweed 
         ,POTCRI = curlyleaf    
         ,STUPEC = sago #maybe should be Stuckenia sp.       
         ,CABCAR = cabomba      
         ,ELOCAN = elodea       
         ,POTRIC = richardson  
         ,NAJGUA = snaiad       
         ,POTPUS = ppusillus    
         ,ECHBER = echinodorus  
         ,VALAUS = ribbonweed   
         ,HETDUB = hdubia  
         ,comments
         #,Egeria_densa = egeria
         #,Ceratophyllum_demersum = coontail     
         #,Myriophyllum_spicatum = watermilfoil 
         #,algae        
         #,Potamogeton_nodosus = am_pondweed 
         #,Potamogeton_crispus = curlyleaf    
         #,Stuckenia_pectinata = sago #maybe should be Stuckenia sp.       
         #,Cabomba_caroliniana = cabomba      
         #,Elodea_canadensis = elodea       
         #,Potamogeton_richardsonii = richardson  
         #,Najas_guadalupensis = snaiad       
         #,Potamogeton_pusillus = ppusillus    
         #,Echinodorus_berteroi = echinodorus  
         #,Vallisneria_australis = ribbonweed   
         #,Heteranthera_dubia = hdubia   
  ) %>% 
  #create new column that calculates total rake coverage of spp
  #should all be either 0 or 100
  rowwise() %>% 
  mutate(tot_cover_spp = sum(c_across(EGEDEN:HETDUB),na.rm=T)) %>% 
  #make new columns that will allow for comparison of rake coverage to spp level coverage
  #values for rake_teeth_logical and tot_cover_spp_logical should match
  #ie, both be zero or both be one; mismatch indicates errors
  mutate(rake_teeth_logical = if_else(rake_teeth > 0, 1,0)
         ,tot_cover_spp_logical = if_else(tot_cover_spp > 0, 1,0)
         ,rake_diff = rake_teeth_logical-tot_cover_spp_logical
  ) %>% 
  #for now, remove cases where mismatch between rake teeth and rake diff (n=110)
  #at some point, Shruti needs to check the sample photos to fix these
  filter(rake_diff==0)

#look for cases in which values for rake_teeth_logical and tot_cover_spp_logical don't match
#note that I made corrections to some of the rake_teeth above but those would still show up
#in the rake_diff search 
#check <- rake_format %>% 
 # filter(rake_diff!=0)
#110 samples have this issue
#no cases of rake_teeth being zero and spp cover being non-zero
#all are cases in which rake_teeth is non-zero while there is no spp specific cover data
#nearly all are rake_teeth = 100% with no spp specific cover
#in some cases, there are helpful notes but should just send these to Shruti who has the sample photos
#write_csv(check,"Data_Raw/CSTARS_GroundTruthing/CSTARS_rakes_mismatches.csv")
#for now, just removed these in code above

#histogram showing all SAV rake cover values
#should all be between 0 and 1
#hist(rake_format$rake_teeth)
#range(rake_format$rake_teeth)
#-1.11  1.00
#overall looks very good but why negative numbers?

#take a closer look at negative numbers for rake teeth
#rake_neg <- rake_format %>% 
#  filter(rake_teeth < 0)
#one sample that has negative value (ie, -1.11)
#fixed this in code above

#histogram showing sum of all spp coverage on rake
#should be either 0 or 100
#hist(rake_format$tot_cover_spp)
#cover_count<-rake_format %>%
#  group_by(tot_cover_spp) %>%
#  summarize(count = n())
#most are either zero or 100 but there are various others
#spp present at trace amounts are included as 0.01 so
#reasonable to see numbers like 0.01-0.03 or 1.01-1.03
#probably shouldn't be numbers like 0.10-0.99 or 1.10-2.0

#how many of these total proportions are clearly wrong
#cover_count2 <- cover_count %>% 
#  filter((tot_cover_spp > 0.03 & tot_cover_spp < 1.00)| tot_cover_spp > 1.03)
#sum(cover_count2$count) #138 samples
#write_csv(cover_count,"Data_Raw/CSTARS_GroundTruthing/CSTARS_rake_prop_errors.csv")

#look at samples with no SAV
#open_water <- rake_format %>% 
#  filter(tot_cover_spp_logical==0)
#111 cases but in all cases rake_teeth >0
#confirmed with Shruti that open water samples excluded from data set

#convert to long format
rake_long <- rake_format %>% 
  #drop some unneeded columns
  select(-c(comments:rake_diff)) %>% 
  pivot_longer(cols=c(EGEDEN:HETDUB)
               ,names_to = "species", values_to = "rake_prop") %>% 
  mutate(
    #add column that indicates whether any SAV was in sample
    sav_incidence = case_when(rake_teeth > 0 ~ 1, 
                                   rake_teeth == 0 ~ 0)
    #add column indicating whether a species was present in a sample
    ,species_incidence = case_when(rake_prop > 0 ~ 1, 
                                        rake_prop == 0 ~ 0)
    #format rake teeth column; sav_rake_prop is probably clearer name
    #,sav_rake_prop = num(rake_teeth,digits=2)
    #rename column with species data too
    #,species_prop = num(rake_prop,digits=2)
    #decided to use a single column with % of rake covered by each species
    #instead of separate columns for total sav prop and spp prop
    #I think this is how FRP does it too
    ,species_rake_cover_percent = (rake_teeth*rake_prop)*100
    ,species_rake_cover_ordinal = case_when(species_rake_cover_percent ==0 ~ 0
                                            ,species_rake_cover_percent >0 & species_rake_cover_percent<= 19~1
                                            ,species_rake_cover_percent >19 & species_rake_cover_percent<= 39~2
                                            ,species_rake_cover_percent >39 & species_rake_cover_percent<= 59~3
                                            ,species_rake_cover_percent >59 & species_rake_cover_percent<=79~4
                                            ,species_rake_cover_percent > 79 ~ 5
    )
         ) %>% 
  #add a couple columns 
  add_column(program = "CSTARS"
             ,sample_method = "rake_rope") %>% 
  #reorder columns
  select(program
         ,sample_method
         ,sample_id
         ,latitude_wgs84
         ,longitude_wgs84
         ,sample_date
         #,sample_time_pdt
         ,sav_incidence
         #,sav_rake_prop
         ,species_code = species
         ,species_incidence
         #,species_prop
         ,species_rake_cover_percent
         ,species_rake_cover_ordinal
         ) %>% 
  arrange(sample_id,species_code) %>% 
  glimpse()

#final check for missing data
#rake_na <- rake_long %>% 
#  filter_all(any_vars(is.na(.)))
#just some missing times, which is fine

#export final file
#write_csv(rake_long,"Data_Formatted/cstars_flatfile.csv")

#Create summary of number of samples in which each species was present-------------
#will use this to indicate which taxa were found in this survey

sp_prev <- rake_long %>% 
  #filter out cases of spp absences
  filter(species_incidence!=0) %>% 
  group_by(species_code) %>% 
  summarize(CSTARS = sum(species_incidence))

#export table
#write_csv(sp_prev,"./Data_Formatted/cstars_spp_summary.csv") 


#examine samples with no SAV
#water <- rake_long %>% 
#  filter(sav_incidence == 0 | sav_rake_prop==0)
#no cases which has to be wrong
#unless they were excluded from the EDI data set, there should be some 
  
#make sure all ssp numbers are in 0-1 range
#hist(rake_long$species_prop)
#looks good but hard to see non-zero bars due to high zero bar
#hist(rake_long$species_prop[rake_long$species_prop!=0])
#sp_cover_count<-rake_long %>%
#  group_by(species_prop) %>%
#  summarize(count = n())
#more unique values than expected 
#no ideal because of inconsistecies in degree of rounding
#but probably no that big an issue as long as they are accurate
#leave these as they are for now

#format the patch level data----------------------

patch_format <- sav_rake %>% 
  select(sample_id 
         ,sample_date = date2
         ,sample_time_pdt = time 
         ,latitude_wgs84 = latitude
         ,longitude_wgs84 = longitude
         ,patch_length_m = pat_len
         ,patch_width_m = pat_wid
         #,location #position of boat relative to patch
         #,position_in #location of sample within patch
         ,preassigned
         ,dep_to_sav   
         ,secchi       
         ,sc_at_bot 
         ) %>% 
  mutate(
    #delete one cell for depth to sav because it contains a bunch of characters
    depth_to_sav = case_when(station==1081 ~ as.numeric(NA)
                                , TRUE ~ as.numeric(dep_to_sav))
    #convert feet to meters
    ,depth_to_sav_m = num(depth_to_sav*0.3048,digits = 2)
    ,secchi_m = num(secchi*0.3048,digits = 2)
    ) %>% 
  #add a couple columns 
  add_column(program = "CSTARS")  %>% 
  #drop unneeded columns and reorder columns
  select(program
         ,sample_id
         ,latitude_wgs84
         ,longitude_wgs84
         ,sample_date
         ,sample_time_pdt
         ,preassigned
         ,depth_to_sav_m
         ,patch_length_m
         ,patch_width_m
         #,location
         #,position_in
         ,secchi_m  
         ,secchi_at_bottom = sc_at_bot
         ) %>% 
  arrange(sample_id) %>% 
  glimpse()

#export patch table
#write_csv(patch_format,"Data_Formatted/cstars_groundtruthing_patch.csv")

#explore patch length and width columns
#unique(patch_format$patch_length_m)
#unique(patch_format$patch_width_m)
#text is awkward but consistent for both

#explore the location and position_in columns
#are they non-overlapping?
#position_check <- patch_format %>% 
#  filter(is.na(location) & is.na(position_in))
#438 cases in which both are NA
#position_check2 <- patch_format %>% 
#  filter(!is.na(location) & !is.na(position_in))
#2256 cases of data in both columns and they don't always match
#ask Shruti to explain

#plot the two depth columns
#hist(patch_format$depth_to_sav_m)
#range(patch_format$depth_to_sav_m,na.rm=T) 
#-0.91  3.05, why negative numbers? above water line?

#look closer at negative depths
#sav_depth <- patch_format %>% 
#  filter(depth_to_sav_m<0)
#sav_depth <- patch_format %>% 
#  filter(station==1081)
#one case; looks like a bunch of data was put into one cell
#removed contents of this cell above (ie, made NA)

#hist(patch_format$secchi_m)
#range(patch_format$secchi_m,na.rm=T) 
#0.00 19.20

#map the coordinates -------------------
#make sure there are no obvious errors

sav_geo <- sav_rake %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  #add year column
  mutate(year=as.factor(str_sub(yyyymm,1,4)))


#create object from bounding box for the stations
#add a buffer around points to improve map aesthetic
#NOTE: this takes a long time to run 
bbox_p <- st_bbox(st_buffer(sav_geo,2000))

#look at WW_Delta base map CRS
#st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
WW_Delta_4326 <- st_transform(WW_Delta, crs = 4326)

(sav_map_ <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot station locations using different colors for different years
    geom_sf(data= sav_geo, aes(fill= year, color=year), size= 2)+
    #set bounding box for site
    coord_sf(
      xlim = c(bbox_p$xmin, bbox_p$xmax),
      ylim = c(bbox_p$ymin, bbox_p$ymax)
    ) +
    theme_bw()
)
