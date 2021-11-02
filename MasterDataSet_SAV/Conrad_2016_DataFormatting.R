#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Conrad et al 2016 study

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

# Read in the data----------------------------------------------
# Data set is on SharePoint site for the 
# Delta Smelt Resiliency Strategy Aquatic Weed Control Action

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Conrad_etal_2016"
  )
)  

#read in the data 
conrad <- read_excel(path=paste0(sharepoint_path,"./ConradEtAl_SAVRakeData_200810.xlsx")
                     ,.name_repair = make_clean_names) #from janitor package
#got some warnings; probably just for parts of data set with lots of NAs where there should be numbers

# Formatting data set--------------------

#date range
#range(conrad$gps_date) #"2008-10-07 UTC" "2010-10-18 UTC"

#look at comment column
comments <- conrad %>% 
  drop_na(comment)
unique(conrad$comment)
#looks like comments didn't get read in
#the few comments in the data set note occasions in which there is no biomass data
#these are generally obvious in the data set even without comments (NA instead of zero for mass)

#explore depth data
hist(as.numeric(conrad$depth))
range(as.numeric(conrad$depth),na.rm = T) #0.1 14.8
#rake handle is only 4.8 m long so exclude any depth measurements over 4.8 m

#start formatting
cleaner <- conrad %>% 
  #drop samples with no-no rake
  #use spp richness column to make sure no samples with SAV got mislabeled as no-no rake
  #the extra code recovers one mislabeled sample
  filter(!(sample=="no-no rake" & (spp_rich==0 | is.na(spp_rich) )))%>% 
  #drop cases where spp richness is NA because it indicates no presence/absence data for sample
  #note that sample type "yes" and "no-no SAV" aren't perfectly accurate so ignore them
  filter(!(is.na(spp_rich))) %>% 
  #change depth column from logical to numeric
  mutate(across(c("depth"), as.numeric)) %>% 
  #drop any cases in which depth is above the rake handle length (max sampling depth)
  filter(depth < 4.8) %>% 
  #dropped unneeded columns
  select(-c("rake_key"
            ,"site_id"
            ,"site_name"
            ,"rake"
            ,"feat_name"
            ,"comment"
            ,"spp_rich":"s_lineage"
  )
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
               values_to = "biomass_fresh_g" 
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
               values_to = "biomass_dry_g" 
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
nsav_f<- long %>% 
  filter(sample=="no-no SAV" & is.na(biomass_fresh_g)) 
#9 NAs; all from WOO_1_061410_R4

#look at NAs for dry biomass
nsav_d<- long %>% 
  filter(sample=="no-no SAV" & is.na(biomass_dry_g)) 
#9 NAs; all from WOO_1_061410_R4
#WOO_1_061410_R4: needs NAs changed to zeros for fresh and dry
#this is done in code below

#look at pa for all samples 
sav_pa<- long %>% 
  filter(is.na(pa)) 
unique(sav_pa$rake_id)
#"WAR_1_060909_R4" 
#NA for pa should be converted to zero
#done in code below

#look at fresh biomass for all samples
sav_f<- long %>% 
  filter(is.na(biomass_fresh_g)) 
unique(sav_f$rake_id) #24 samples
#20 samples from WOO_1 and VIC_1 6/14/2010, all but one of which should be NAs for wet and dry mass
#exception: WOO_1_061410_R4 is a no SAV sample so we know mass is zeros for all species
#plus "DIS_2_101209_R7": one NA that should be a zero
#plus "BIG_1_102009_R3": one NA that should be a zero 

#look at dry biomass for all samples
sav_d<- long %>% 
  filter(is.na(biomass_dry_g)) 
unique(sav_d$rake_id) #22 samples
#20 samples from WOO_1 and VIC_1 6/14/2010, all but one of which should be NAs for wet and dry mass
#exception: WOO_1_061410_R4 is a no SAV sample so we know mass is zeros for all species

#look at cases of missing GPS coordinates
lost <- long %>% 
  filter(is.na(easting)|is.na(northing))
lost_samp<-unique(lost$rake_id) #284 samples are missing coordinates
lost_date<-unique(lost$gps_date) #all samples from 9 dates in Oct 2010 
#this issue is noted in original metadata for this data set

#samples are generally taken at the same rake numbers each date,
#so maybe we can copy coordinates from other dates

#are there samples with missing dates? probably not
miss_date <- long %>% 
  filter(is.na(gps_date))
#no missing dates

#change species nicknames to latin names

# Making data frame with existing strings and their replacement
tr <- data.frame(target = c("egde",  "caca",  "pocr",  "mysp",  "cede",  "stspp", "elca",  "pono",  "stfi"),
                 replacement = c("Egeria_densa","Cabomba_caroliniana","Potamogeton_crispus","Myriophyllum_spicatum","Ceratophyllum_demersum","Stuckenia_sp","Elodea_canadensis","Potamogeton_nodosus","Stuckenia_filiformis")
                 )

# Making the named replacement vector from tr
replacements <- c(tr$replacement)
names(replacements) <- c(tr$target)

#make corrections based on data exploration above and do some more formatting/cleaning
long_cleaner <- long %>% 
  #one sample with NA for pa was were determined to be zero so convert from NA
  replace_na(list(pa=0)) %>% 
  #fresh and dry mass for this sample should be zeros instead of NAs (no SAV present)
  mutate(across(.cols = c(biomass_fresh_g,biomass_dry_g),
                .fns = ~case_when(rake_id == "WOO_1_061410_R4" 
                                  ~ as.numeric(0),
                                  TRUE ~ as.numeric(.x)))) %>% 
  #in this sample, one species was NA for fresh biomass but should be zero
  mutate(across(.cols = c(biomass_fresh_g),
                .fns = ~case_when(rake_id == "DIS_2_101209_R7" & species == "pocr" ~ as.numeric(0),
                                  TRUE ~ as.numeric(.x)))) %>% 
  #in this sample, one species was NA for fresh biomass but should be zero
  mutate(across(.cols = c(biomass_fresh_g),
                .fns = ~case_when(rake_id == "BIG_1_102009_R3" & species == "stfi" ~ as.numeric(0),
                                  TRUE ~ as.numeric(.x)))) %>% 
  #change species nicknames to latin names
  mutate(species1 = str_replace_all(species,pattern = replacements)) %>%
  #drop the sample column which is now redundant with the presence/absence column data
  #also drop the species nickname column now that we have latin names column
  select(-c("sample","species")) %>% 
  #drop rows with missing GPS coordinates
  #see if there is a logical way to fill in some of these
  filter(!is.na(easting) & !is.na(northing)) %>% 
  #rename some columns 
  rename("id"="rake_id"
         ,"water_depth_m" = "depth"
         ,"incidence"="pa"
         ,"species"="species1"
         ,"date"="gps_date") %>% 
  #add columns that identify these data as part of this survey
  add_column("program"=as.character("electrofishing_nearshore")
             ,"survey_method"=as.character("rake_thatch")
             ) %>% 
  #add density columns; divides mass by area sampled by rake (0.101 m^2)
  mutate("density_fresh_g_m^2"=round((biomass_fresh_g/0.101),2)
         ,"density_dry_g_m^2"=round((biomass_dry_g/0.101),2)
         ) %>% 
  #change CRS of sample coordinates
  #specify the CRS of original coordinate: UTM NAD 83 (Zone 10N) (EPSG = 26910)
  st_as_sf(coords = c("easting", "northing"), crs = 26910) %>%
  #then transform to WGS84
  st_transform(4236) %>% 
  #then convert from geometry to columns
  mutate(latitude_wgs84 = unlist(map(geometry,2)),
         longitude_wgs84 = unlist(map(geometry,1))) %>% 
  #drop the geometry column
  st_set_geometry(NULL) %>% 
  #reorder columns
  select("program"
         #,"id"
         , "date"
         , "latitude_wgs84"
         , "longitude_wgs84"
         , "species"
         , "survey_method"
         #probably don't need "incidence" now
         #this info should be captured by 0 or NA
         #but double check this 
         #, "incidence" 
         , "biomass_fresh_g"
         , "biomass_dry_g"
         ,"density_fresh_g_m^2"
         ,"density_dry_g_m^2"
         , "water_depth_m"
         ) %>% 
  glimpse()

#map all sampling locations

#look at WW_Delta base map CRS
#st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points
#EPSG: 4269

#prep sample coordinates for mapping
gps <- long_cleaner %>% 
  #select needed columns
  select("date" 
         ,"latitude_wgs84"
         ,"longitude_wgs84"
  ) %>% 
  #reduce to just unique combinations of date and location
  distinct(date,longitude_wgs84,latitude_wgs84) %>% 
  #converting coordinates back to sf object
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84') 
           ,crs = 4236
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

# Define path on SharePoint site for output files
sharepoint_path_write <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
) 
#write the formatted dataset to sharepoint site
#write_csv(long_cleaner,file = paste0(sharepoint_path_write,"/NearshoreEfishing_2008-2010_formatted.csv"))













