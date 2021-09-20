#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#Conrad et al 2016 study

#Notes
#there are water quality data associated with some of these samples but aren't in this data set
#no WQ data for October 2008
#get these from Louise
#as Louise for how dry mass per m^2 was calculated
#dissolved oxygen (mg/L), pH, specific conductance (μS), temperature (°C), and turbidity (NTU)
#are rake numbers within site same location across dates?
#dropped 284 samples with missing GPS coordinates; see if there is a logical way to fill in some of these


# Packages
library(tidyverse) #suite of data science tools
library(readxl) #read excel files
library(janitor) #clean up column names
library(sf) #convert GPS coordinate units

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

#look at categories for sample column
unique(conrad$sample)
#"no-no SAV", "yes", "no-no rake"
#drop "no-no rake" because these are spots too deep to sample 
#quite possible no SAV in these spots but can't know for sure either way

#look at comment column
comments <- conrad %>% 
  drop_na(comment)
unique(conrad$comment)
#looks like comments didn't get read in
#the few comments in the data set note occasions in which there is no biomass data
#these are obvious in the data set even without comments (NA instead of zero for mass)
#But in (at least) three cases, there was no SAV but data were NA when should be zero 
#SAC_1_061809_R7, STE_1_061809_R8, WOO_1_061410_R4

cleaner <- conrad %>% 
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
  #change depth column from logical to numeric
  mutate(across(c("depth"), as.numeric)) %>% 
  #drop sites that were too deep to sample
  filter(sample!="no-no rake") %>% 
  #drop sites where sample = yes but all or nearly all SAV columns contain NAs (n=33)
  filter(!(sample=="yes" & is.na(egde))) %>% 
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

#for presence/absence, there are only two options: zero or one
#because only sites that were sampled are included here
#those where sampling was attempted but failed because of deep water were already dropped

#see if there are any cases of no SAV detected but data logged as NA instead of zero
nsav_pa<- long %>% 
  filter(sample=="no-no SAV" & is.na(pa)) #16 rows 
#all from two samples

#similarly, look at NAs for fresh biomass
nsav_f<- long %>% 
  filter(sample=="no-no SAV" & is.na(biomass_fresh_g)) 
#25 rows, so 9 additional NAs beyond the ones for those two samples

#similarly, look at NAs for dry biomass
nsav_d<- long %>% 
  filter(sample=="no-no SAV" & is.na(biomass_dry_g)) 
#25 rows; same number as fresh biomass

unique(nsav_f$rake_id)
unique(nsav_d$rake_id)
#same three samples 
#SAC_1_061809_R7: needs NAs changed to zeros for pa, fresh, and dry
#STE_1_061809_R8: needs NAs changed to zeros for pa, fresh, and dry
#WOO_1_061410_R4: needs NAs changed to zeros for fresh and dry (pa is zeros already)
#this is done in code below

#look at pa for all samples (not just those without SAV)
sav_pa<- long %>% 
  filter(is.na(pa)) #17
unique(sav_pa$rake_id)
#two are same as above: "SAC_1_061809_R7" "STE_1_061809_R8"
#one additional one: "WAR_1_060909_R4" 
#for all three samples, these NAs for pa should be converted to zeros
#done in code below

#look at fresh biomass for all samples
sav_f<- long %>% 
  filter(is.na(biomass_fresh_g)) 
unique(sav_f$rake_id) #24 samples
#20 samples from WOO_1 and VIC_1 6/14/2010, all but one of which should be NAs for wet and dry mass
#exception: WOO_1_061410_R4 is a no SAV sample so we know mass is zeros for all species
#plus "SAC_1_061809_R7" "STE_1_061809_R8", which should be zeros
#plus "DIS_2_101209_R7": one NA that should be a zero
#plus "BIG_1_102009_R3": one NA that should be a zero 

#look at dry biomass for all samples
sav_d<- long %>% 
  filter(is.na(biomass_dry_g)) 
unique(sav_d$rake_id) #22 samples
#20 samples from WOO_1 and VIC_1 6/14/2010, all but one of which should be NAs for wet and dry mass
#exception: WOO_1_061410_R4 is a no SAV sample so we know mass is zeros for all species
#plus "SAC_1_061809_R7" "STE_1_061809_R8", which should be zeros

#look at cases of missing GPS coordinates
lost <- long_cleaner %>% 
  filter(is.na(easting)|is.na(northing))
lost_samp<-unique(lost$id) #284 samples are missing coordinates
#if samples are taken at the same rake numbers each date,
#then maybe we can copy coordinates from other dates
#if each sample location is unique, we should drop all these samples with missing coordinates

#are there samples with missing dates? probably not
miss_date <- long_cleaner %>% 
  filter(is.na(date))
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
  #all three samples with NAs for pa were were determined to be zeros so convert from NA
  replace_na(list(pa=0)) %>% 
  #fresh and dry mass for these three samples should be zeros instead of NAs (no SAV present)
  mutate(across(.cols = c(biomass_fresh_g,biomass_dry_g),
                .fns = ~case_when(rake_id == "SAC_1_061809_R7" | rake_id == "STE_1_061809_R8" | rake_id == "WOO_1_061410_R4" 
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
  #drop rows with missing GPS coordinates; see if there is a logical way to fill in some of these
  filter(!is.na(easting) & !is.na(northing)) %>% 
  #rename some columns 
  rename("id"="rake_id"
         ,"depth_m" = "depth"
         ,"presence_absence"="pa"
         ,"species"="species1"
         ,"date"="gps_date") %>% 
  #add columns that identify these data as part of this survey
  add_column("survey"=as.character("electrofishing_nearshore")
             ,"survey_method"=as.character("thatch_rake")
             ) %>% 
  #add density columns; divides mass by area sampled by rake (0.101 m^2)
  #these don't match the ones from the original file so check with Louise
  mutate("density_fresh_g_m^2"=round((biomass_fresh_g/0.101),2)
         ,"density_dry_g_m^2"=round((biomass_dry_g/0.101),2)
         ) %>% 
  #reorder columns
  select("survey"
         ,"id"
         , "date"
         , "northing"
         , "easting"
         , "species"
         , "survey_method"
         , "presence_absence"
         , "biomass_fresh_g"
         , "biomass_dry_g"
         ,"density_fresh_g_m^2"
         ,"density_dry_g_m^2"
         , "depth_m"
         )

#convert northing/easting to latitude/longitude; might need to ask Louise for more info
#actually could try to figure it out, map points, and see if they match article map
long_gps<-long_cleaner %>%
  #British National Grid probably has EPSG code 27700 but look this up to be sure
  st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  #presumably the code below is for WGS84
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble()
#this creates a df with only the lat/long data
#work on this so it add these as columns to main df

#look at total number of unique rake IDs
#the number of rows should be 9x that number (ie, 9 species)
rake_ids <- unique(long_cleaner$rake_id)
#3874 x 9 = 34866 (which is the number of rows in df)

#explore depth data a bit
hist(long_cleaner$depth_m)
range(long_cleaner$depth_m,na.rm = T) #0.10, 5.02














