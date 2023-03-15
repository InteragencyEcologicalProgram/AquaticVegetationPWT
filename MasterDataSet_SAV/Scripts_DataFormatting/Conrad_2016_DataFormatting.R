#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Conrad et al 2016 study

#To do list--------

#decide what to do about Stuckenia; probably just label all Stuckenia as S. pectinata
#go through and decide what should be zeros vs NA for things like mass 
#if species_incidence=1 and no mass, then NA; if species_incidence=0, then mass=0

#Notes
#are rake numbers within site same location across dates? Yes
#need to confirm this by looking closer at data
#if true, then create a station column from ID column (ie, site_site#_rake)

#dropped 284 samples with missing GPS coordinates; see if there is a logical way to fill in some of these

#rake dimensions from Conrad et al 2016
#handle was 4.8 m long, so that is pretty much max possible sampling depth
#rake head was 35 cm long, so when spun on axis this is the circle diameter
#area sampled is then 3.14 *(0.35m/2)^2 = 0.101 m^2

#sample labeling isn't perfectly accurate so don't rely on it: yes, no_no rake, and no_no sav
#drop cases of no_no rake but make sure not to drop those mislabeled as no_no rake

# Packages
library(tidyverse) #suite of data science tools
library(readxl) #read excel files
library(janitor) #clean up column names
library(sf) #convert GPS coordinate units
library(deltamapr) #Sam's package with delta base layers
library(lubridate) #formatting dates

# Read in the data----------------------------------------------

#read in the sav data from bass study 
conrad <- read_excel(path="./Data_Raw/Conrad_etal_2016/bass_study_data.xlsx") %>%
  clean_names() %>% 
  glimpse()

#ready in taxonomy data set to get species codes
taxonomy <- read_csv("./Data_Formatted/taxonomy_all.csv") %>% 
  select(species,species_code)

# Formatting data set--------------------

#date range
#range(conrad$gps_date) #"2008-10-07 UTC" "2010-10-18 UTC"

#look at comment column
#comments <- conrad %>% 
#  drop_na(comment)
#unique(conrad$comment)
#looks like comments didn't get read in
#the few comments in the data set note occasions in which there is no biomass data
#these are generally obvious in the data set even without comments (NA instead of zero for mass)

#explore depth data
#hist(as.numeric(conrad$depth))
#range(as.numeric(conrad$depth),na.rm = T) #0.1 14.8
#rake handle is only 4.8 m long so maybe exclude any depth measurements over 4.8 m
#maybe the high values were in feet? 14.8 could also be a typo for 4.8 m
#don't include depth in integrated sav data set, so don't filter these samples out for now

#look at species richness
#hist(conrad$spp_rich)

#start formatting
cleaner <- conrad %>% 
  #drop samples with no-no rake (couldn't reach bottom with rake)
  #use spp richness column to make sure no samples with SAV got mislabeled as no-no rake
  #the extra code recovers one mislabeled sample
  filter(!(sample=="no-no rake" & (spp_rich==0 | is.na(spp_rich) ))) %>% 
  #drop cases where spp richness is NA because it indicates no presence/absence data for sample
  #could either be mislabeled no-no SAV or yes samples missing species composition
  #note that sample type "yes" and "no-no SAV" aren't perfectly accurate so ignore them
  filter(!(is.na(spp_rich)))  %>% 
  mutate(
    #add sav incidence column
    "sav_incidence" = case_when(spp_rich==0~0,spp_rich>0~1)
    #change depth column from logical to numeric
    ,across(c("depth"), as.numeric)
    ) %>% 
  #drop any cases in which depth is above the rake handle length (max sampling depth)
  #actually this drops a lot of samples and we don't include depth in integrated data set
  #so don't filter these out for now
  #filter(depth < 4.8) 
  #dropped unneeded columns
  select(
    rake_id:easting
    ,sav_incidence
    ,sav_mass_fresh_g = total_fw
    #,depth
    ,sample
    ,egde:stfi_dw
  ) %>% 
  glimpse()

#convert wide to long
#probably not best solution but just break into three parts for simplicity

#start with presence/absence portion
pa <- cleaner %>% 
  select(rake_id:stfi) %>% 
  pivot_longer(cols = c(egde:stfi), 
               names_to = "species", 
               values_to = "pa" 
               )

#then wet mass portion
wet <- cleaner %>% 
  select(c("rake_id",contains("_fw"))) %>% 
  pivot_longer(cols = c(egde_fw:stfi_fw), 
               names_to = "species1", 
               values_to = "species_mass_fresh_g" 
  ) %>% 
  #drop "_fw" from strings in species column
  mutate(species = str_replace_all(species1,"_fw","")) %>% 
  #drop the unneeded column
  select(-"species1")

#then dry mass portion
dry <- cleaner %>% 
  select(c("rake_id",contains("_dw"))) %>% 
  pivot_longer(cols = c(egde_dw:stfi_dw), 
               names_to = "species1", 
               values_to = "species_mass_dry_estimated_g" 
  ) %>% 
  #drop "_dw" from strings in species column
  mutate(species = str_replace_all(species1,"_dw","")) %>% 
  #drop the unneeded column
  select(-"species1")

#rejoin the three df's
long <- list(pa,wet,dry) %>% 
  reduce(left_join, by=c("rake_id","species")) %>% 
  glimpse()

#look at NAs for fresh biomass
#nsav_f<- long %>% 
#  filter(sample=="no-no SAV" & is.na(species_mass_fresh_g)) 
#9 NAs; all from WOO_1_061410_R4

#look at NAs for dry biomass
#nsav_d<- long %>% 
#  filter(sample=="no-no SAV" & is.na(species_mass_dry_estimated_g)) 
#9 NAs; all from WOO_1_061410_R4
#WOO_1_061410_R4: needs NAs changed to zeros for fresh and dry
#this is done in code below

#look at pa for all samples 
#sav_pa<- long %>% 
#  filter(is.na(pa)) 
#unique(sav_pa$rake_id)
#"WAR_1_060909_R4" 
#NA for pa should be converted to zero
#done in code below

#look at fresh biomass for all samples
#sav_f<- long %>% 
#  filter(is.na(species_mass_fresh_g)) 
#unique(sav_f$rake_id) #24 samples
#20 samples from WOO_1 and VIC_1 6/14/2010, all but one of which should be NAs for wet and dry mass
#exception: WOO_1_061410_R4 is a no SAV sample so we know mass is zeros for all species
#plus "DIS_2_101209_R7": one NA that should be a zero
#plus "BIG_1_102009_R3": one NA that should be a zero 

#look at dry biomass for all samples
#sav_d<- long %>% 
#  filter(is.na(species_mass_dry_estimated_g)) 
#unique(sav_d$rake_id) #22 samples
#20 samples from WOO_1 and VIC_1 6/14/2010, all but one of which should be NAs for wet and dry mass
#exception: WOO_1_061410_R4 is a no SAV sample so we know mass is zeros for all species

#look at cases of missing GPS coordinates
#lost <- long %>% 
#  filter(is.na(easting)|is.na(northing))
#lost_samp<-unique(lost$rake_id) #284 samples are missing coordinates
#lost_date<-unique(lost$gps_date) #all samples from 9 dates in Oct 2010 
#this issue is noted in original metadata for this data set

#samples are generally taken at the same rake numbers each date,
#so maybe we can copy coordinates from other dates

#are there samples with missing dates? probably not
#miss_date <- long %>% 
#  filter(is.na(gps_date))
#no missing dates

#change species nicknames to latin names

# Making data frame with existing strings and their replacement
#NOTE: made Stuckenia sp into Stuckenia pectinata; supposedly all Delta Stuckenia are S. pectinata; maybe should change S. filiformis too
tr <- data.frame(target = c("egde",  "caca",  "pocr",  "mysp",  "cede",  "stspp", "elca",  "pono",  "stfi"),
                 replacement = c("Egeria_densa","Cabomba_caroliniana","Potamogeton_crispus","Myriophyllum_spicatum","Ceratophyllum_demersum","Stuckenia_pectinata","Elodea_canadensis","Potamogeton_nodosus","Stuckenia_filiformis")
                 )

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

#make corrections based on data exploration above and do some more formatting/cleaning
long_cleaner <- long %>% 
  #one sample with NA for pa was determined to be zero so convert from NA
  replace_na(list(pa=0)) %>% 
  #fresh and dry mass for this sample should be zeros instead of NAs (no SAV present)
  mutate(across(.cols = c(species_mass_fresh_g,species_mass_dry_estimated_g),
                .fns = ~case_when(rake_id == "WOO_1_061410_R4" 
                                  ~ as.numeric(0),
                                  TRUE ~ as.numeric(.x)))) %>% 
  #in this sample, one species was NA for fresh biomass but should be zero
  mutate(across(.cols = c(species_mass_fresh_g),
                .fns = ~case_when(rake_id == "DIS_2_101209_R7" & species == "pocr" ~ as.numeric(0),
                                  TRUE ~ as.numeric(.x)))) %>% 
  #in this sample, one species was NA for fresh biomass but should be zero
  mutate(across(.cols = c(species_mass_fresh_g),
                .fns = ~case_when(rake_id == "BIG_1_102009_R3" & species == "stfi" ~ as.numeric(0),
                                  TRUE ~ as.numeric(.x)))) %>% 
  #change species nicknames to latin names
  mutate(species1 = str_replace_all(species,pattern = replacements)) %>%
  #drop the sample column which is now redundant with the presence/absence column data
  #also drop the species nickname column now that we have latin names column
  select(-c("sample","species")) %>% 
  #drop rows with missing GPS coordinates
  #see if there is a logical way to fill in some of these
  #actually let's leave these in; at least we know which site they're from
  #for some purposes the coordinates might not be critical
  #filter(!is.na(easting) & !is.na(northing)) %>% 
  #rename some columns 
  rename("sample_id"="rake_id"
         #,"water_depth_m" = "depth"
         ,"species_incidence"="pa"
         ,"species"="species1") %>% 
  #add columns that identify these data as part of this survey
  add_column("program"=as.character("BASS")
             ,"sample_method"=as.character("rake_handle")
             ) %>% 
  mutate(
    #change datetime to date
    "sample_date" = as_date(gps_date)
    #add density columns; divides mass by area sampled by rake (0.101 m^2)
    ,"species_density_fresh_g_m2"=round((species_mass_fresh_g/0.101),2)
    ,"species_density_dry_estimated_g_m2"=round((species_mass_dry_estimated_g/0.101),2)
         ) %>% 
  #change CRS of sample coordinates
  #specify the CRS of original coordinate: UTM NAD 83 (Zone 10N) (EPSG = 26910)
  st_as_sf(coords = c("easting", "northing"), crs = 26910,na.fail = F) %>%
  #then transform to WGS84
  st_transform(4326) %>% 
  #then convert from geometry to columns
  mutate(latitude_wgs84 = unlist(map(geometry,2)),
         longitude_wgs84 = unlist(map(geometry,1))) %>% 
  #drop the geometry column
  st_set_geometry(NULL) %>% 
  #add standardized species codes to replace species
  left_join(taxonomy) %>% 
  #reorder columns
  select("program"
         , "sample_method"
         ,"site" = "site_name"
         ,"sample_id"
         , "latitude_wgs84"
         , "longitude_wgs84"
         , "sample_date"
         ,"sav_incidence"
         ,"sav_mass_fresh_g"
         , "species_code"
         #probably don't need "species_incidence" now
         #this info should be captured by 0 or NA
         #but double check this 
         , "species_incidence" 
         , "species_mass_fresh_g"
         , "species_mass_dry_estimated_g"
         ,"species_density_fresh_g_m2"
         ,"species_density_dry_estimated_g_m2"
         #, "water_depth_m"
         ) %>% 
  glimpse()

#write the formatted dataset 
#write_csv(long_cleaner,file = "./Data_Formatted/bass_flatfile.csv")

#Create summary of number of samples in which each species was present-------------
#just need to export the list of taxa
#will use this to indicate which taxa were found in this survey

sp_prev <- long_cleaner %>% 
  #filter out cases of spp absences
  filter(species_incidence!=0) %>% 
  group_by(species_code) %>% 
  summarize(BASS = sum(species_incidence))

#export table
#write_csv(sp_prev,"./Data_Formatted/bass_spp_summary.csv") 



#map all sampling locations-----------------------

#look at WW_Delta base map CRS
#st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#prep sample coordinates for mapping
gps <- long_cleaner %>% 
  #select needed columns
  select("sample_date" 
         ,"latitude_wgs84"
         ,"longitude_wgs84"
  ) %>% 
  #reduce to just unique combinations of date and location
  distinct(sample_date,longitude_wgs84,latitude_wgs84) %>% 
  #converting coordinates back to sf object
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84') 
           ,crs = 4326
           ,remove=F
           ) %>%  #EPSG code for WGS84
  #transform to NAD83 (basically same as WGS84 though)
  st_transform(crs = 4269) 

#plot bay-delta base layer with sample locations 
(sav_map_all <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot all sampling locations from all dates
    geom_sf(data= gps, fill= "red", color= "black", shape= 22, size= 3.5) +
    #zoom in on region of delta where samples were collected
    coord_sf( 
     xlim =c(min(gps$longitude_wgs84)-0.1,max(gps$longitude_wgs84)+0.1),
     ylim =c(min(gps$latitude_wgs84)-0.1,max(gps$latitude_wgs84)+0.1) 
     )+
    theme_bw()+
    ggtitle('Nearshore Electrofishing Study')
)
#compared with map in Fig. 1 of Conrad et al 2016
#sampling locations look correctly placed















