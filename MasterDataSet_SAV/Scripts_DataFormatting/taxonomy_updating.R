#Aquatic Vegetation Project Work Team
#Integrated data set
#Submersed aquatic vegetation
#look up taxonomic information in online databases
#use the taxize package to automate this

# Packages--------
library(tidyverse) #suite of data science tools
library(janitor) #make column names tidier

#read in taxonomy info  -----------

#survey specific taxon abundances
bass <- read_csv("./Data_Formatted/bass_spp_summary.csv")
cstars <- read_csv("./Data_Formatted/cstars_spp_summary.csv")
dsrs <- read_csv("./Data_Formatted/dsrs_spp_summary.csv")
franks <- read_csv("./Data_Formatted/franks_spp_summary.csv")

#all taxa with TSN and origin
itis <- read_csv("./Data_Formatted/taxonomy_all_itis.csv") %>% 
  glimpse()

#join all files by species code------------

#combine survey info
dfs <- list(bass,cstars,dsrs,franks)
comb <- dfs %>% 
  reduce(full_join) %>% 
  #replace NA with zeros
  replace(is.na(.),0) %>% 
  #sort by species code
  arrange(species_code)

#now combine survey data with full taxonomy
tcomb <- left_join(itis,comb)

#write file
#write_csv(tcomb,"./Data_Formatted/taxonomy_all_summary.csv")


