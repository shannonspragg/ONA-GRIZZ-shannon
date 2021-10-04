# Creating the Farm Type Data Frame ---------------------------------------
# This is just a consolidated version of the Ag Census Data manipulation done to
# create a Farm Type data frame and then pull out the Dominant Farm Type by CCS

# Load Packages -----------------------------------------------------------
library("tidyverse")
library("dplyr")
library("raster")
library("sp")
library("sf")

# Import Files ------------------------------------------------------------
farm.type <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/farm type_32100403/farm type_32100403.csv")

# I found the .shp for Consolidated Census Subdivisions (CCS)!! Read in Below:
can.ccs.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN census cons subdivisions (CCS)/lccs000b16a_e.shp")


# Filter CCS and Ag Files to BC Only ---------------------------------------------------
# Make sf and filter down to only British Columbia for Census SubDivs (CCS):

can.ccs.sf<- as(can.ccs.shp, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"
bc.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()
st_write(bc.ccs, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census subdivs/BC CCS.shp")

# Filter the Ag Files down to just BC districts:
# See here: https://www.statology.org/filter-rows-that-contain-string-dplyr/  searched: 'Return rows with partial string, filter dplyr'
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 

# Filtering to just the BC regions with a CCS number:
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))

# Writing this as a .csv to save us the trouble of doing this over later
write_csv(bc.farm.filter.ccs, "bc_farms_filtered.ccs.csv")

unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., grepl("2016", bc.farm.filter.ccs$REF_DATE)) # Now there are 344 observations


# Editing CCS Code into new column for join -------------------------------
# Seperate out the CCS code into new column for join with CCS .shp:
bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # There we go, now we have a matching 6 digits
unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
str(bc.farm.2016.ccs)
str(bc.farm.filter)


# Joining the CCS with the Farm Type: -------------------------------------
# Join the BC CCS with Ag Files:
farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 
unique(farm.ccs.join$GEO)

str(bc.ccs) # Classes sf and data.table
str(bc.farm.2016.ccs)
# Got this to go through, now to view it!
str(farm.ccs.join) # Here we have a dataframe with Multipolygon geometry - check!

# Trying to write these for easier use later
st_write(farm.ccs.join,"farm_type_ccs.shp")
write_csv(farm.ccs.join, "farm_type_ccs.csv") # There is something funky with this table.. ?

# Selecting Dominant Ag Types By Region -----------------------------------
# Going to pull out the top 2 farm count values for each of the CCS codes 
# https://statisticsglobe.com/select-row-with-maximum-or-minimum-value-in-each-group-in-r

farm.ccs.join$VALUE<- as.integer(farm.ccs.join$VALUE)
# Pull out the top two values from the join:
top.two.ccs.farm.types<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE)  # YAY! This worked!
str(top.two.ccs.farm.types)

# Subset the Farm Join & Print --------------------------------------------

# Try putting this in the console: farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE)[2,]

# RUN THIS NEXT!! (Try with and without comma after bracket)
top.two.ccs.farm.types<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE)[2,]

# TRY THIS: https://stackoverflow.com/questions/2453326/fastest-way-to-find-second-third-highest-lowest-value-in-vector-or-column
library("Rfast")
dominant.ccs.farm.type <- Rfast::nth(farm.ccs.join, 2, descending = T)

# Extract Total Farm Counts by CCS: (Do this with the dominant farms too)
total.farms.bc<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE) %>% slice_min(., order_by = "VALUE")
# This successfully gives us total farm count by CCS region
dominant.farms.bc<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE) %>% slice_tail()
# YUSS! This gives us the dominant type WITHOUT the total farms :))


write_csv(dominant.farms.bc, "Dominant Farm Types by CCS.csv")
st_write(dominant.farms.bc,"Dominant Farm Types by CCS.shp")

write_csv(total.farms.bc, "Total Farm Count by CCS.csv")
st_write(total.farms.bc, "Total Farm Count by CCS.shp") #These map well!



