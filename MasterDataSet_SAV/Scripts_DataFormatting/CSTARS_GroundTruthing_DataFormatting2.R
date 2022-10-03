#Aquatic Vegetation Project Work Team
#Master data set
#Submersed aquatic vegetation
#CSTARS ground truthing
#raw data pulled from EDI

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #work with GPS coordinates
library(janitor) #make column names tidier
library(lubridate) #formatting dates

#read in data from EDI
#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1143
sav_rake <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1143.1&entityid=c818f952f9401a32678484fea246dc5a") %>% 
  #automatically clean column name format
  clean_names()
#glimpse(sav_rake)

#format data set for integration
#will create two tables
#one that is patch level data
#the other spp level data

#format spp level data table
rake_format <- sav_rake %>% 
  #only keep spp level columns and rename them as needed
  select(station = orig_fid
         ,date
         ,time_pst = time #check previous script to confirm time zone
         ,latitude_wgs84 = latitude
         ,longitude_wgs84 = longitude
         ,rake_teeth = rk_tth
         ,Egeria_densa = egeria
         ,Ceratophyllum_demersum = coontail     
         ,Myriophyllum_spicatum = watermilfoil 
         ,algae        
         ,Potamogeton_nodosus = am_pondweed 
         ,Potamogeton_crispus = curlyleaf    
         ,Stuckenia_pectinata = sago #maybe should be Stuckenia sp.       
         ,Cabomba_caroliniana = cabomba      
         ,Elodea_canadensis = elodea       
         ,Potamogeton_richardsonii = richardson  
         ,Najas_guadalupensis = snaiad       
         ,Potamogeton_pusillus = ppusillus    
         ,Echinodorus_berteroi = echinodorus  
         ,Vallisneria_australis = ribbonweed   
         ,Heteranthera_dubia = hdubia   
         ,comments
  ) %>% 
  #create new column that calculates total rake coverage of spp
  #should all be either 0 or 100
  rowwise() %>% 
  mutate(tot_cover_spp = sum(c_across(Egeria_densa:Heteranthera_dubia),na.rm=T)) %>% 
  #make new columns that will allow for comparison of rake coverage to spp level coverage
  #values for rake_teeth_logical and tot_cover_spp_logical should match
  #ie, both be zero or both be one; mismatch indicates errors
  mutate(rake_teeth_logical = if_else(rake_teeth > 0, 1,0)
         ,tot_cover_spp_logical = if_else(tot_cover_spp > 0, 1,0)
         ,rake_diff = rake_teeth_logical-tot_cover_spp_logical
  )

#look for cases in which values for rake_teeth_logical and tot_cover_spp_logical don't match
#note that I made corrections to some of the rake_teeth above but those would still show up
#in the rake_diff search 
check <- rake_format %>% 
  filter(rake_diff!=0)
#110 samples have this issue
#no cases of rake_teeth being zero and spp cover being non-zero
#all are cases in which rake_teeth is non-zero while there is no spp specific cover data
#nearly all are rake_teeth = 100% with no spp specific cover
#in some cases, there are helpful notes but should just send these to Shruti who has the sample photos





