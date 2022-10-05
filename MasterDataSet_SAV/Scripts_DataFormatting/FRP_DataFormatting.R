#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#CSTARS ground truthing
#includes all Delta points 
#does not include Suisun Marsh points
#raw data pulled from EDI

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

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
frp_site <-read_csv("Data_Raw/FRP/tables_all/SiteVisit.csv") %>% 
  clean_names()

#sample level data like date, location, water depth
frp_sample <-read_csv("Data_Raw/FRP/tables_all/Sample.csv")%>% 
  clean_names()

#data for species on rake samples
frp_veg <-read_csv("Data_Raw/FRP/tables_all/Vegetation.csv")%>% 
  clean_names()

#crosswalks numeric species codes and latin names and higher level taxonomy
#will only need the subset of these that are for SAV species
frp_code <-read_csv("Data_Raw/FRP/tables_all/VegetationCode.csv")%>% 
  clean_names()
#NOTE: I manually added a column that indicates whether a species is SAV

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
    ,sample_id_key
    ,vegetation_id
    ,species_code = vegetation_code
    ,area
    ,wet_weight
    ,dry_weight
    ,health_code
  ) %>% 
  glimpse()

#how many occurrences of each species? 

#should check to see if area % within a sample add to 100

#create taxonomy table of just the SAV species in the field data--------

taxonomy_field <- veg_format %>% 
  distinct(species_code)
#44 of 67 species in original vegetation code file

taxonomy <- left_join(taxonomy_field,frp_code, by=c("species_code"="vegetation_code"),keep=F)
#definitely stuff in here that isn't SAV
#are these all rake samples or is this all survey data









