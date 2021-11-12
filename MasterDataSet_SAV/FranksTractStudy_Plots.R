#Drought Barrier Report
#Franks Tract Study
#plots 

#To do list
#there seem to be NAs present but seems like there shouldn't be; look closer at those


#packages
library(tidyverse) # wrangling tabular data and plotting
library(plotrix) #calcuate standard error


# Define path on SharePoint site for data
# I synced this folder to my OneDrive
dir_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
)  

#read in the sample data
cleandat <- read_csv(file=paste0(dir_path,"./FranksTractManagement_2014-2021_formatted.csv"))
#glimpse(cleandat) #looks good

#read in the data showing species origin
origin <- read_csv(file=paste0(dir_path,"./FranksTractManagement_SpeciesOrigin.csv"))

#combine sample and spp origin dfs
dato <- left_join(cleandat,origin)

#summarize number of samples by year
ssize <- dato %>% 
  distinct(date,station) %>% 
  group_by(date) %>% 
  summarize(samples = n(), .groups = 'drop') 
range(ssize$samples) #45 200

#summarize data set to show number of occurrences of each spp by year
#and the mean score for each spp and year
occur <- dato  %>% 
  group_by(date,species) %>% 
  summarize(sp_freq = sum(species_incidence)
            ,avg_score = mean(rake_coverage_ordinal)
            , .groups = 'drop')  

#add sample number to spp occurrence df
occurs <- left_join(occur,ssize)

#calculate proportion of occurrences for each spp by year
poccurs <- occurs %>% 
  rowwise() %>% 
  mutate(sp_freq_prop = sp_freq/samples)

#add spp origin to this data set
pcsn <- left_join(poccurs,origin)

#calculate proportion of occurrences for each spp 
#mostly want this to determine how best to order spp in bar graphs
occura <- dato  %>% 
  group_by(species) %>% 
  summarize(sp_freq = sum(species_incidence), .groups = 'drop') %>% 
  arrange(-sp_freq)

#use spp column from this df as list to specify order of bars in plot below
ssp_order <- occura$species

pcsn$species <- factor(pcsn$species
                          , levels= c("Potamogeton_richardsonii",  "Ceratophyllum_demersum",    "Egeria_densa"             
                          ,"Najas_guadalupensis"       ,"Elodea_canadensis"        , "Stuckenia_filiformis"     
                           ,"Stuckenia_pectinata"      , "Potamogeton_crispus"       ,"Potamogeton_foliosus"     
                           ,"Myriophyllum_spicatum"   ,  "Nitella_sp"               , "Potamogeton_pusillus"     
                           ,"Potamogeton_zosteriformis", "Potamogeton_nodosus"      , "Heteranthera_dubia" 
                            ))


#plot proportion of occurrences of each species by year
#using proportion instead of number because sample sizes vary among years
(plot_spp_occur_prop <-ggplot(pcsn
                              , aes(x=species, y= sp_freq_prop, fill=native))+
    geom_bar(stat = "identity") + 
    ylab("Proportional Occurrence") + xlab("Species") + 
    facet_wrap(~date, nrow=8)
      )
#to do
#turn date into year
#angle spp names so they are readable
#could lump rare taxa into "other" category (eg, any with ten or fewer occurrences)

#plot of mean score by species and year----------------

#add spp origin to this data set
meansc <- left_join(occur,origin)

#calculate mean score for each spp 
#mostly want this to determine how best to order spp in bar graphs
mn_sp <- dato  %>% 
  group_by(species) %>% 
  summarize(sp_avg_score = mean(rake_coverage_ordinal, na.rm=T), .groups = 'drop') %>% 
  arrange(-sp_avg_score)

#use spp column from this df as list to specify order of bars in plot below
ssp_order2 <- mn_sp$species

meansc$species <- factor(meansc$species
                       , levels= c("Potamogeton_richardsonii",  "Ceratophyllum_demersum",    "Egeria_densa"             
                                   ,"Najas_guadalupensis"       ,"Elodea_canadensis"        , "Stuckenia_filiformis"     
                                   ,"Stuckenia_pectinata"      , "Potamogeton_crispus"       ,"Potamogeton_foliosus"     
                                   ,"Myriophyllum_spicatum"   ,  "Nitella_sp"               , "Potamogeton_pusillus"     
                                   ,"Potamogeton_zosteriformis", "Potamogeton_nodosus"      , "Heteranthera_dubia" 
                       ))

#plot mean scores of each species by year
(plot_spp_score_avg <-ggplot(meansc
                              , aes(x=species, y= avg_score, fill=native))+
    geom_bar(stat = "identity") + 
    ylab("Mean Ordinal Score") + xlab("Species") + 
    facet_wrap(~date, nrow=8)
)
#add standard error bars

#could try to make a plot that has scores of all spp within samples summed and then calculate mean scores by year
score_sum <- dato  %>% 
  group_by(date,station) %>% 
  summarize(tot_score = sum(rake_coverage_ordinal)
            , .groups = 'drop')  %>% 
  group_by(date) %>% 
  summarize(tot_score_avg = mean(tot_score, na.rm=T)
            ,tot_score_se = std.error(tot_score, na.rm=T)
            , .groups = 'drop')

#plot mean scores by year
(plot_score_avg <-ggplot(score_sum
                             , aes(x=date, y= tot_score_avg))+
    geom_bar(stat = "identity", fill="dark green") + 
    geom_errorbar(aes(ymin=tot_score_avg-tot_score_se, ymax=tot_score_avg+tot_score_se)
                  #, width=.1
                  ) +
    ylab("Mean Ordinal Score") + xlab("Year") 
)





