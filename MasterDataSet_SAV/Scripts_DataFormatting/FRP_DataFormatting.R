#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#CSTARS ground truthing
#includes all Delta points 
#does not include Suisun Marsh points
#raw data pulled from EDI

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#Nick to do list---------------

#what are units for weight? grams?

#difference between veg categories: OTHER, UNKNO, SAVGEN

#are species that are included as zeros trace amounts?
#supposed to be a ones but could be a zero

# Packages--------
library(tidyverse) #suite of data science tools
library(readxl) #read excel files
library(janitor) #make column names tidier
library(lubridate) #formatting dates
library(sf) #work with GPS coordinates
library(deltamapr) #maps of the delta

#read in data from excel files-----------
#decided not to use these because they are missing some important fields that allow
#tables to relate

#site level data for day of visit like weather and water quality
#frp_site1 <-read_excel("Data_Raw/FRP/tables_needed/FRP_SiteVisit_29apr2022.xlsx") %>% 
#  clean_names()
#same number of rows as csv version but fewer columns

#sample level data like date, location, water depth
#frp_sample1 <-read_excel("Data_Raw/FRP/tables_needed/FRP_Sample_29apr2022.xlsx")%>% 
#  clean_names()
#much fewer rows and columns than csv version

#data for species on rake samples
#frp_veg1 <-read_excel("Data_Raw/FRP/tables_needed/FRP_Vegetation_29apr2022.xlsx")%>% 
#  clean_names()
#same number of rows and columns as csv version

#crosswalks numeric species codes and latin names and higher level taxonomy
#will only need the subset of these that are for SAV species
#frp_code1 <-read_excel("Data_Raw/FRP/tables_needed/FRP_VegetationCode_29apr2022.xlsx")%>% 
#  clean_names()
#same number of rows and columns as csv version

#read in data from csv files--------------------
#original files that contain all columns

#site level data for day of visit like weather and water quality
#connected to sample level data via visit_no column
frp_site <-read_csv("Data_Raw/FRP/tables_all/SiteVisit.csv") %>% 
  clean_names()

#sample level data like date, location, water depth
#connected to species level data via sample_id_key
frp_sample <-read_csv("Data_Raw/FRP/tables_all/Sample.csv")%>% 
  clean_names()

#data for species on rake samples
#vegetation_id is unique identified for every row (ie, species within sample)
#connected to species codes via vegetation_code_id
frp_veg <-read_csv("Data_Raw/FRP/tables_all/Vegetation.csv")%>% 
  clean_names()

#crosswalks numeric species codes and latin names and higher level taxonomy
#will only need the subset of these that are for SAV species
frp_code <-read_csv("Data_Raw/FRP/tables_all/VegetationCode.csv")%>% 
  clean_names()
#NOTE: I manually added a column that indicates whether a species is SAV

#explore sample data set-------------

unique(frp_sample$gear_type_abbreviation)


#format the species level data--------

#explore visit_no column
visits <-unique(frp_veg$vist_no)
#n = 350, but lots of NAs too
#this matches to site visit level data

#explore wet and dry weight columns
hist(frp_veg$wet_weight)
hist(frp_veg$dry_weight)

#join species abundance data with taxonomy 
veg_format <- left_join(frp_veg ,frp_code) %>% 
  #drop unneeded columns
  select(
    visit_no = vist_no
    ,sample_id = sample_id_key
    ,vegetation_id
    ,species_code = vegetation_code
    ,type = vegetation_type
    ,area
    ,wet_weight
    ,dry_weight
    ,health_code
  ) %>% 
  arrange(vegetation_id) %>% 
  glimpse()

#look at hydrilla sample
hydrilla <- veg_format %>% 
  filter(sample_id==6902)
#90% is "hydrilla"; clearly an error
#this sample also includes Azolla (FAV) so it gets dropped out of final data set
#which is good

#look at OPEWAT samples
water<-veg_format %>% 
  filter(species_code=="OPEWAT") %>% 
  arrange(area)
#for vegetation_id 4036, the sample_id (3445) is incorrectly in the sample_id column

#look at example of sample with open water
water_ex<-veg_format %>% 
  filter(visit_no==4861) 

#how many occurrences of each species? 
spp_occur <- veg_format %>% 
  group_by(species_code,type) %>% 
  summarise(count= n()) %>% 
  arrange(type,-count)

#probably should filter out any samples that aren't just SAV
#need to summarize samples by veg type first
type_sum <-veg_format %>% 
  group_by(sample_id,type) %>% 
  summarise(count=n()) %>% 
  arrange(sample_id) %>% 
  pivot_wider(names_from = type ,values_from = count)

#sav only samples
#keep these
sav_only <- type_sum %>% 
  filter(!is.na(s) & is.na(o)& is.na(f)) %>% 
  pull(sample_id)
#1421 samples
1421/1864 #76%

#sav + fav only samples
#consider keeping these
sav_fav <-  type_sum %>% 
  filter(!is.na(s) & is.na(o)& !is.na(f))
#6 samples so not much loss in tossing them

#sav + other
#toss
sav_other <- type_sum %>% 
  filter(!is.na(s) & !is.na(o)& is.na(f))
#245 samples

#fav samples
#toss these
fav_only <- type_sum %>% 
  filter(is.na(s) & is.na(o) & !is.na(f))
#12 samples
12/1864 #0.6%

#other only samples
#toss these
other_only <- type_sum %>% 
  filter(is.na(s) & !is.na(o) & is.na(f))
#175 samples with only other species (not sav)
160/1864 #9%

#all types
#toss
mixed <-type_sum %>% 
  filter(!is.na(s) & !is.na(o) & !is.na(f))
#17 samples

#format SAV only samples------------------

sav_only_samples <- veg_format %>% 
  filter(sample_id %in% sav_only) %>%
  #change percent to proportion
  mutate(species_prop = area/100
         ,species = case_when(species_code == "ELODIA" ~ "ELOCAN"
                              ,species_code == "OPEWAT"~ "EMPTY"
                              ,species_code == "NADA" ~ "EMPTY"
                              ,TRUE ~ species_code)
         ) %>% 
  #drop unneeded columns: visit_no (all NAs),type (all SAV now), area (replaced by species_prop),
  #species code (replaced by species), 
  #heath_code (lots missing and remaining are strings of multiple categories - mostly healthy)
  select(sample_id
         ,vegetation_id
         ,species
         ,species_prop
         #,health_code
         ,biomass_fresh = wet_weight #what are units?
         ,biomass_dry = dry_weight #what are units?
      )

#look closer at columns with lots of NAs
#health <- sav_only_samples %>% 
#  filter(!is.na(health_code))
#many cells contain multiple health codes separated by semi colon
#mostly healthy
#for now just leave this column out

#did this fiter work correctly? are there 1421 unique sample_id?
#sav_check <-unique(sav_only_samples$sample_id)
#yes 


#look at non-specific SAV categories
misc <- sav_only_samples %>% 
  filter(species == "OTHER" | species == "UNKNO" | species == "SAVGEN") %>% 
  arrange(species)
#only two cases of SAVGEN and they are 100% of sample in both cases to just drop those samples

#look closer at OTHER and UNKNO
misc_more <- sav_only_samples %>% 
  select(sample_id,species,species_prop) %>% 
  pivot_wider(id_cols = sample_id,names_from = species,values_from = species_prop)

#look at duplicates
sav_dup <- sav_only_samples%>%
  dplyr::group_by(sample_id, species) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>% 
  pull(sample_id)

#look at samples with duplicates
sav_dup_samp <- sav_only_samples %>% 
  filter(sample_id %in% sav_dup)



#additional SAV formatting and cleaning
sav_format <-sav_only_samples %>% 
  #drop rows that are complete duplicates (n=2)
  distinct(sample_id,species,species_prop,biomass_fresh,biomass_dry) %>%
  #drop samples that are entirely or almost entirely OTHER (n=2)
  filter(!(sample_id== 8874 | sample_id== 8875)) %>% 
  #fix one duplicate by summarizing by sample and species
  group_by(sample_id,species) %>% 
  summarize(species_prop2 = sum(species_prop)
            #I don't think either biomass will change
            ,biomass_fresh2 = sum(biomass_fresh)
            ,biomass_dry2 = sum(biomass_dry)
            , .groups = "drop") %>% 
  glimpse()
  #lost the biomass columns though
  
#look at samples with duplicates
sav_dup_samp2 <- sav_format %>% 
  filter(sample_id %in% sav_dup)

  


#should check to see if area % within a sample add to 100

#create taxonomy table of just the SAV species in the field data--------

taxonomy_field <- sav_format %>% 
  distinct(species)
#15 of 67 species in original vegetation code file
#consider combining OTHER, UNKNO, SAVGEN which might effectively be all the same
#also consider just dropping samples with these categories but should look at them first
#UNKNO and OTHER might not even be SAV 



#taxonomy <- left_join(taxonomy_field,frp_code, by=c("species_code"="vegetation_code"),keep=F)
#definitely stuff in here that isn't SAV
#are these all rake samples or is this all survey data









