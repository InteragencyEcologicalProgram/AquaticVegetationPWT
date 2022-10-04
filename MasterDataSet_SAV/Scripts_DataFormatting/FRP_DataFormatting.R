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

#read in data-----------

#site level data for day of visit like weather and water quality
frp_site <-read_excel("Data_Raw/FRP/tables_needed/FRP_SiteVisit_29apr2022.xlsx") %>% 
  clean_names()

#sample level data like date, location, water depth
frp_sample <-read_excel("Data_Raw/FRP/tables_needed/FRP_Sample_29apr2022.xlsx")%>% 
  clean_names()

#data for species on rake samples
frp_veg <-read_excel("Data_Raw/FRP/tables_needed/FRP_Vegetation_29apr2022.xlsx")%>% 
  clean_names()

#crosswalks numeric species codes and latin names and higher level taxonomy
#will only need the subset of these that are for SAV species
frp_code <-read_excel("Data_Raw/FRP/tables_needed/FRP_VegetationCode_29apr2022.xlsx")%>% 
  clean_names()













