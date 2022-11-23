#Aquatic Vegetation Project Work Team
#Integrated data set
#Submersed aquatic vegetation
#Delta Smelth Resiliency Strategy Aquatic Weed Control Action
#most data pulled from EDI

#To do list-----------

#decide where to put sample_method column
#should be at program level but for now more practical in sample level

#as starting point, using tidy format with different tables for info
#consider making a version that is one big flat file so users don't have to do joining
#the exercise of combining all the separate tables would be a good QAQC check too

#add station number as a column because this was a repeated measures study
#problem is that probably wasn't recorded so would have to do some spatial analysis to figure it out

#could add a survey month table
#could include WQ data by calculating monthly summary stats from sonde data
#could also include monthly fluridone treatment data

#for sample level data, could reincorporate the health level data
#this wasn't included in the original EDI package but we have it

# Packages--------
library(tidyverse) #suite of data science tools
library(janitor) #make column names tidier
library(lubridate) #formatting dates
library(sf) #work with GPS coordinates
library(deltamapr) #maps of the delta

#read in data-----------

#DSRS data is on EDI 

#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1079.1

sav_rake_data <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1079.1&entityid=f29d1162866d4e9fb2fdd7355efd4c1e") %>%
  glimpse()
#NOTE: didn't use clean_names() here because need the species names in current format for steps below

#read in fluridone sampling station data from EDI
#will use some of these stations for site level location data
fl_station <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1079.1&entityid=a8ca1295eace8c7709ef1c65eec16748") %>% 
  clean_names()

#read in species file to get the species codes
sav_taxonomy <- read_csv("./Data_Formatted/taxonomy_all.csv") %>% 
  select(species_code,species)

#create sample IDs--------------------------
#eventually should use the ID #s of the stations as part of this because it 
#was a repeated measures design

sav_rake <- sav_rake_data %>% 
  arrange(Date,Site,Latitude,Longitude) %>% 
  #just use a sequence of numbers with DSRS prefix
  mutate(
    program = "DSRS"
    ,number = seq(1:876)
    ,sample_id = str_c(program,number)
    ) %>%
  glimpse()

#Format site level data-------------
#site, site abbreviation, site coordinates, treatment status
#may not be analogous tables for other data sets

#first create df with site names and abbrev.
site_names <- data.frame(
  cbind(
    "program"=rep("DSRS",4)
    ,"site" = c("decker_island","fishermans_cut","little_hastings","french_island")
    ,"site_code" = c("DI","FC","LH","FI")
  ))

#grab site level coordinates from fluridone station df
#use the station for each site closest to center of site
site_coords <- fl_station %>% 
  filter(
    (site=="DI" & station=="SAV") |
      (site=="FC" & station=="SAV") |
      (site=="LH" & station == "Center" & type=="water_sediment") |
      (site=="FI" & station=="Center")
  ) %>% 
  select(site_code = site, latitude,longitude)

#grab start and end date for survey within each site
survey_range <- sav_rake %>% 
  clean_names() %>% 
  group_by(site) %>% 
  summarize(survey_start = min(date)
            ,survey_end = max(date)
            ) %>% 
  rename(site_code = site)

#join names,coordinates,and dates into one df using site code
site_list <- list(site_names,site_coords,survey_range)
site_meta <- site_list %>% 
  reduce(full_join)

site_level <- sav_rake %>% 
  #automatically clean column name format
  clean_names() %>%
  #only keep the needed columns
  select(site_code = site,site_type) %>% 
  #distill to site level 
  distinct() %>% 
  #create some columns 
  mutate(
    #columns that make herbicide use clearer
    sav_control = case_when(site_type=="Treated" ~ "y",TRUE ~ "n")
    ,control_tool = case_when(site_type=="Treated" ~ "fluridone",TRUE ~ "NA")
         ) %>% 
  #add rest of site info
  right_join(site_meta)  %>% 
  #only keep needed columns
  select(program,site,site_code,site_latitude_wgs84 = latitude,site_longitude_wgs84=longitude,survey_start,survey_end,sav_control,control_tool)

#export table
#write_csv(site_level,"./Data_Formatted/dsrs_site.csv")

#Format sample level data---------------
#this table is useful for folks who just want to know about SAV abundance (not spp)
#site, date, lat, long, total biomass
#decided to leave out survey_month column (eg, 2017_06)

sample_level <- sav_rake %>% 
  #automatically clean column name format
  clean_names() %>% 
  mutate(
    #add column that indicates sampling method
    sample_method = "rake_thatch"
    #convert mass from kg to g for consistency across data sets
    ,sav_mass_fresh_g = total_wet_biomass_kg*1000
    #create column that indicates whether there was SAV in a sample
    ,sav_incidence = case_when(sav_mass_fresh_g==0~0,TRUE~1)
         ) %>% 
  #only keep needed columns
  select(site_code=site,sample_id,sample_method,sample_date=date,latitude_wgs84=latitude,longitude_wgs84=longitude,sav_incidence,sav_mass_fresh_g)

#export table
#write_csv(sample_level,"./Data_Formatted/dsrs_sample.csv")

#Format species within sample level data------------------
#station, species specific rake cover and estimated wet mass
#convert wide to long

species_level <- sav_rake %>% 
  #only keep the needed columns
  select(sample_id,Total_Wet_Biomass_kg,Egeria_densa:Cabomba_caroliniana) %>% 
  #convert wide to long
  pivot_longer(cols=(Egeria_densa:Cabomba_caroliniana),names_to = "species",values_to = "rake_cover_percent") %>% 
  mutate(
    #create new column with estimated species specific biomass 
    species_mass_fresh_estimated_g = (Total_Wet_Biomass_kg*1000) * (rake_cover_percent/100)
    #create column that indicates whether a species was present in a sample
    ,species_incidence = case_when(rake_cover_percent==0~0,TRUE~1)
    ) %>% 
  #add the species name abbrev.
  left_join(sav_taxonomy) %>% 
  #automatically clean column name format
  clean_names() %>%
  #only keep the needed columns and rename code as species
  select(sample_id,species_code,species_incidence,rake_cover_percent,species_mass_fresh_estimated_g)

#look for cases in which names didn't join properly (ie, code = NA)
#name_na <- species_level %>% 
#  filter(is.na(code))
#nope, joining was done correctly for all rows

#export table
#write_csv(species_level,"./Data_Formatted/dsrs_sample_species.csv")

#Create summary of number of samples in which each species was present-------------
#just need to export the list of taxa
#will use this to indicate which taxa were found in this survey

sp_prev <- species_level %>% 
  #filter out cases of spp absences
  filter(species_incidence!=0) %>% 
  group_by(species_code) %>% 
  summarize(dsrs = sum(species_incidence))

#export table
#write_csv(sp_prev,"./Data_Formatted/dsrs_spp_summary.csv")

#create flat file version of data----------------------------
#combine all tables

#reduce number of columns for site level
#many of these are high level metadata columns that don't make sense when combined with rest of data
site_trunc <- site_level %>% 
  select(program,site,site_code)

#combine site and sample tables using site code
stsp <- right_join(site_trunc,sample_level)  %>% 
  select(-site)

#add the species level data using sample ID
all <- right_join(stsp,species_level) %>% 
  select(program
         ,sample_method
         ,site = site_code
         ,sample_id
         ,latitude_wgs84
         ,longitude_wgs84
         ,sample_date
         ,sav_incidence:species_mass_fresh_estimated_g
         )

#export table
#write_csv(all,"./Data_Formatted/dsrs_flatfile.csv")

















