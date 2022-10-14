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

#why are there mass values for NADA rows?

#should review all the notes in files at some point

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

#unique(frp_sample$gear_type_abbreviation)


#format the species level data--------

#explore visit_no column
#visits <-unique(frp_veg$vist_no)
#n = 350, but lots of NAs too
#this matches to site visit level data

#explore wet and dry weight columns
#hist(frp_veg$wet_weight)
#hist(frp_veg$dry_weight)

#look at rows with mass
vmass1 <- frp_veg %>% 
  filter(vegetation_code_id== 28 & (!is.na(wet_weight) | !is.na(dry_weight)))
#write_csv(vmass1,"./Data_Raw/FRP/data_issues/frp_nada_masses.csv")

#look at visit and sample IDs
#I think these columns are used interchangeably
#any cases of number in both visit and sample in same row?
#vs <- frp_veg %>% 
#  filter(!is.na(vist_no) & !is.na(sample_id_key))
#no rows with a number in both columns as expected

#join species abundance data with taxonomy 
veg_format <- left_join(frp_veg ,frp_code) %>% 
  #sometimes sample id was put into the visit id column
  #create a new column that puts all the sample id's into one column
  #after coalescing, I checked and found no NAs in this new column so it worked fine
  mutate(sample_id = coalesce(vist_no, sample_id_key))  %>% 
  #drop unneeded columns
  select(
    sample_id 
    ,species_code = vegetation_code
    ,type = vegetation_type
    ,area
    ,wet_weight
    ,dry_weight
  ) %>% 
  glimpse()

#look at hydrilla sample
hydrilla <- veg_format %>% 
  filter(sample_id==6902)
#90% is "hydrilla"; clearly an error
#this sample also includes Azolla (FAV) so it gets dropped out of final data set
#which is good


#how many occurrences of each species? 
spp_occur <- veg_format %>% 
  group_by(species_code,type) %>% 
  summarise(count= n()) %>% 
  arrange(type,-count)

#look at samples based on vegetation type composition------------

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

#all types in a sample
#toss
mixed <-type_sum %>% 
  filter(!is.na(s) & !is.na(o) & !is.na(f))
#17 samples

#format SAV only samples------------------

submersed_only_samples <- veg_format %>% 
  filter(sample_id %in% sav_only)

#explore rows with wet or dry mass values
#includes NADA category which doesn't make sense because that's no veg
vmass <- submersed_only_samples %>% 
  filter(species_code=="NADA" & (!is.na(wet_weight) | !is.na(dry_weight)))

#how many NADA and OPEWAT with value of 1?
trace_wat <- submersed_only_samples %>% 
  filter((species_code == "NADA" | species_code == "OPEWAT") & area ==0)
  #filter((species_code == "NADA" | species_code == "OPEWAT") & (area> 0 & area < 2))
#these two categories were entered as 0 in 108 cases but never as 1
#IMPORTANT: side note, there are wet weight values and even dry weight values 
#for NADA which makes no sense

#what types of species have zero as area
zeros <- submersed_only_samples %>% 
  filter(area==0) %>% 
  distinct(species_code)
#zeros for many different species

sav_only_samples <- submersed_only_samples %>% 
  mutate(
    #change percent to proportion
    species_proportion = area/100
    #clean up some codes in species column
    #Format Elodea name so same as others
    #NADA and OPEWAT are interchangeable to make same
    ,species = case_when(species_code == "ELODIA" ~ "ELOCAN"
                              ,species_code == "OPEWAT"~ "EMPTY"
                              ,species_code == "NADA" ~ "EMPTY"
                              ,TRUE ~ species_code)
    #species at trace amounts were entered as either zero or 0.01; make them all 0.01
    #note that there are samples with EMPTY entered as zero, including those in which EMPTY
    #is the only row (ie, no SAV at all); these need to remain zeros
    ,species_prop = case_when(species!= "EMPTY" & species_proportion ==0  ~ 0.01
                              ,TRUE~species_proportion)
         ) %>% 
  #drop unneeded columns: type (all SAV now), area (replaced by species_prop),
  #species code (replaced by species), 
  select(sample_id
         ,species
         ,species_prop
         ,biomass_fresh = wet_weight #what are units?
         ,biomass_dry = dry_weight #what are units?
      ) %>% 
  glimpse()

#did this filter work correctly? are there 1396 unique sample_id?
#sav_check <-unique(sav_only_samples$sample_id)
#yes 

#look at non-specific SAV categories
misc <- sav_only_samples %>% 
  filter(species == "OTHER" | species == "UNKNO" | species == "SAVGEN") %>% 
  arrange(species)
#these are probably interchangeable

#look at duplicates
sav_dup <- sav_only_samples%>%
  dplyr::group_by(sample_id, species) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>% 
  pull(sample_id)

#look at samples with duplicates
sav_dup_samp <- sav_only_samples %>% 
  filter(sample_id %in% sav_dup) %>% 
  arrange(sample_id,species)

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
  rename(species_prop = species_prop2
         ,biomass_fresh = biomass_fresh2
         ,biomass_dry = biomass_dry2
  ) %>% 
  glimpse()

#look at samples with duplicates again
sav_dup_samp2 <- sav_format %>% 
  filter(sample_id %in% sav_dup) %>% 
  arrange(sample_id,species)
#looks like they are fixed but highlights other issues remaining in data 
#eg, rake total values greater than 1

#look closer at OTHER and UNKNO
misc_more <- sav_format %>% 
  select(sample_id,species,species_prop) %>% 
  pivot_wider(id_cols = sample_id,names_from = species,values_from = species_prop) %>% 
  filter(!is.na(UNKNO) | !is.na(OTHER) | !is.na(SAVGEN))

sav_formatter <- sav_format %>% 
  #SAVGEN, OTHER, and UNKNO look interchangeable so combine into one category
  mutate(species2 = case_when(species=="OTHER" | species == "UNKNO" | species == "SAVGEN"~ "UNKNOW"
                              ,TRUE~species)) %>% 
  select(-species) %>% 
  rename(species = species2)

#need to reformat data so every species has a row in every sample
#do this by converting from long to wide 
#adding an SAV incidence column
#back to long format
#think about what to do with EMPTY category; should probably keep it as a row for each sample 

#should check to see if area % within a sample add to 100
rake_sum <- sav_formatter %>% 
  group_by(sample_id) %>% 
  summarize(rake_prop = sum(species_prop),.groups = "drop")

#quick plot of rake proportions; should all be around 1
#NOTE: species present in trace amounts are entered as 0.01 so values like 1.01 are expected
#Also, this survey uses EMPTY to represent proportion without SAV so shouldn't be any zeros
hist(rake_sum$rake_prop)

#look at number of samples for each rake total
rake_sum_sum <- rake_sum %>% 
  group_by(rake_prop) %>% 
  summarize(count = n())

#look closer at samples with total rake props that aren't 1-1.04
#hard to imagine more than four taxa in a sample at trace amounts
rake_sum_wrong <- rake_sum %>% 
  filter(rake_prop < 1 | rake_prop > 1.04) %>% 
  arrange(rake_prop) 
#n=50 samples
#for sample totals < 1, crew may have just forgotten to enter value for EMPTY
#but could also have entered the wrong proportion for one or more species
#totals of 0.9-0.95 are probably just bad math
#anything much higher than 1 is an error; maybe a typo
#samples that are 1-04-1.05 all included EMPTY as 0.01 which should be removed


#look closer at samples with wrong rake sum totals
rake_sample_wrong <- left_join(rake_sum_wrong, sav_formatter) %>% 
  select(sample_id, rake_prop, species, species_prop) %>% 
  arrange(rake_prop,sample_id)
#7 samples that are only EMPTY and have 0 as value; these should be 1 instead
#can fix these when converting to wide format; when all species prop sum to zero then empty is 1
#other samples have only trace amounts of 1-4 species, so empty should be 1 I guess
#again can fix this in wide format; whenever species total is < 0.05, empty should be 1
#for samples between 0.05 and 0.9, probably just forgot to include EMPTY, 
#so inpute 1 - SAV species total for EMPTY
#two samples that are just EMPTY = 0.10, probably should be 1
#code above should fix these too (ie when SAV species sum to 0, EMPTY=1)
#make sure to leave original EMPTY column and add new one based on rules
#then compare the two EMPTY columns when they differ
#will need to do some rounding possibly for EMPTY calculations because
#of trace species
#many of the samples above 1.06 could also be fixed by recalculating
#value (ie, reducing value) for EMPTY after accounting for species
#most of the samples that are 1.9 and up don't make sense to me; ask Dan

sav_ftr_wide <- sav_formatter %>% 
  #for now drop the mass columns; bring them back later
  select(sample_id,species,species_prop) %>% 
  #make wide
  pivot_wider(names_from = species, values_from = species_prop) %>% 
  #reorder columns so EMPTY is at end
  select(sample_id,STUPEC,CERDEM:POTRIC,EMPTY) %>% 
  #replace NAs with zeros in all SAV species columns 
  mutate_at(vars(STUPEC:EMPTY), replace_na, 0) %>% 
  rowwise() %>% 
  mutate(
    #calculate total rake cover for all SAV species in sample (excludes EMPTY)
    sav_tot = sum(c_across(STUPEC:POTRIC))
    #calcuate new version of EMPTY based on the SAV totals above
    ,EMPTY2 = 1-sav_tot
    #compare the two EMPTY columns
    ,empty_dif = round(EMPTY - EMPTY2,2)
         )

#look at cases where original and newly calculated EMPTY columns differ
empty <- sav_ftr_wide %>% 
  filter(empty_dif!=0) %>% 
  arrange(empty_dif)
#229 samples that don't match
#174 are within 0.06 so might just be due mostly to trace species
#55 remaining probaby similar to those samples that didn't add to 1 above

#create taxonomy table of just the SAV species in the field data--------

taxonomy_field <- sav_formatter %>% 
  distinct(species)
#13 of 67 species in original vegetation code file

taxonomy <- left_join(taxonomy_field,frp_code, by=c("species"="vegetation_code"),keep=F) %>% 
  select(
    species_code = species
    ,family
    ,genus
    ,specific_epithet = species.y
    ,native
  ) %>% 
  arrange(species_code) %>% 
  glimpse()
#note that we changed Elodea code so won't match which is fine because original info wasn't very good
#this file still needs clean up but we'll do that in another script that makes the master taxonomy file
#for all surveys

#write_csv(taxonomy,"./Data_Formatted/frp_taxonomy.csv")







