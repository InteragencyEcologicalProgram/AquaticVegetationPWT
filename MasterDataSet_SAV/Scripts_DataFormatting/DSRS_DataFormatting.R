#Aquatic Vegetation Project Work Team
#Integrated data set
#Submersed aquatic vegetation
#Delta Smelth Resiliency Strategy Aquatic Weed Control Action
#most data pulled from EDI

#To do list-----------

#decide where to put survey_method column
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

#read in CSTARS species file to get the species abbreviations
sav_taxonomy <- read_csv("./Data_Formatted/cstars_taxonomy.csv")
#NOTE: all species in the DSRS samples are already in the cstars taxonomy file

#create sample IDs--------------------------
#eventually should use the ID #s of the stations as part of this because it 
#was a repeated measures design

sav_rake <- sav_rake_data %>% 
  arrange(Date,Site,Latitude,Longitude) %>% 
  #just use a sequence of numbers with DSRS prefix
  mutate(
    program = "DSRS"
    ,number = seq(1:876)
    ,sample = str_c(program,number)
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
    survey_method = "rake_thatch"
    #convert mass from kg to g for consistency across data sets
    ,total_biomass_fresh_g = total_wet_biomass_kg*1000
    #create column that indicates whether there was SAV in a sample
    ,sav_incidence = case_when(total_biomass_fresh_g==0~0,TRUE~1)
         ) %>% 
  #only keep needed columns
  select(site_code=site,sample,survey_method,sample_date=date,sample_latitude_wgs84=latitude,sample_longitude_wgs84=longitude,sav_incidence,total_biomass_fresh_g)

#export table
#write_csv(sample_level,"./Data_Formatted/dsrs_sample.csv")

#Format species within sample level data------------------
#station, species specific rake cover and estimated wet mass
#convert wide to long

#first create version of taxonomy data with just names and name abbrev.
sp_names <- sav_taxonomy %>% 
  select(code,species)

species_level <- sav_rake %>% 
  #only keep the needed columns
  select(sample,Total_Wet_Biomass_kg,Egeria_densa:Cabomba_caroliniana) %>% 
  #convert wide to long
  pivot_longer(cols=(Egeria_densa:Cabomba_caroliniana),names_to = "species",values_to = "rake_cover_percent") %>% 
  mutate(
    #create new column with estimated species specific biomass 
    biomass_fresh_estimated_g = (Total_Wet_Biomass_kg*1000) * (rake_cover_percent/100)
    #create column that indicates whether a species was present in a sample
    ,species_incidence = case_when(rake_cover_percent==0~0,TRUE~1)
    ) %>% 
  #add the species name abbrev.
  left_join(sp_names) %>% 
  #automatically clean column name format
  clean_names() %>%
  #only keep the needed columns and rename code as species
  select(sample,species_code = code,species_incidence,rake_cover_percent,biomass_fresh_estimated_g)

#look for cases in which names didn't join properly (ie, code = NA)
#name_na <- species_level %>% 
#  filter(is.na(code))
#nope, joining was done correctly for all rows

#export table
#write_csv(species_level,"./Data_Formatted/dsrs_sample_species.csv")

#Create summmary of number of samples in which each species was present-------------
#just need to export the list of taxa
#will use this to indicate which taxa were found in this survey

sp_prev <- species_level %>% 
  #filter out cases of spp absences
  filter(species_incidence!=0) %>% 
  group_by(species_code) %>% 
  summarize(dsrs = sum(species_incidence))

#export table
#write_csv(sp_prev,"./Data_Formatted/dsrs_spp_summary.csv")


