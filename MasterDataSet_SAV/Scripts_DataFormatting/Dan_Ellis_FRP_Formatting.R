# FRP SAV Rake - Vegetation Data merge

# Written/Edited by Daniel Ellis
# Last edits: 05 OCT 2022


# Bring in appropriate libraries
library(readxl)
library(tidyr)
library(dplyr)

# Bring on the data...
sample = read_xlsx("FRP_Sample_29apr2022.xlsx", sheet = 1)
sv = read_xlsx("FRP_SiteVisit_29apr2022.xlsx", sheet = 1)
veg = read_xlsx("FRP_Vegetation_29apr2022.xlsx", sheet = 1)
veg.codes = read_xlsx("FRP_VegetationCode_29apr2022.xlsx", sheet = 1)
csv.sample = read.csv("Sample.csv", quote = "")
csv.sv = read.csv("SiteVisit.csv", quote = "")

# Cut data down to that needing merging
sample.merge = csv.sample[,c(which(colnames(csv.sample)=="SampleID_frp")
                             ,which(colnames(csv.sample)=="SampleID_key")
                             ,which(colnames(csv.sample)=="VisitNo"))]

# Merge site VisitNo to sample data
sample = merge(sample, sample.merge, by = "SampleID_frp", all.x = T )

# Find columns that arent matching up
# Note- In the following function, order of df's matters
setdiff(names(csv.sv), names(sv))
setdiff(names(sv), names(csv.sv))

# Rename columns
names(sv)[which(colnames(sv)=="Visit Date")]<-"Visit.Date"


# Merge VisitNo to site visit data
# sv = merge(sv, csv.sv, all.x = T )
# This was not really possible after i removed some columns...go back to origional

# test merge the data without the sv.xlsx - just use origional
test = merge(sample, csv.sv, by = "VisitNo", all.x = T)


Decisions-  # use the .csv for site visits  # Use the above to merge VisitNo from sample.csv to sample.xlsx
  
  