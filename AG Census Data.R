# Importing and mapping the Ag Census Variables -----------------------
# The existing list of variables that I am interested in investigating as predictors
# for grizzly regression are as follows:
# Source: STATCAN AG CENSUS 2016 <https://www150.statcan.gc.ca/n1/en/type/data?cansim=004-0200%2C004-0201%2C004-0202%2C004-0203%2C004-0204%2C004-0205%2C004-0206%2C004-0207%2C004-0208%2C004-0209%2C004-0210%2C004-0211%2C004-0212%2C004-0213%2C004-0214%2C004-0215%2C004-0216%2C004-0217%2C004-0218%2C004-0219%2C004-0220%2C004-0221%2C004-0222%2C004-0223%2C004-0224%2C004-0225%2C004-0226%2C004-0227%2C004-0228%2C004-0229%2C004-0230%2C004-0231%2C004-0232%2C004-0233%2C004-0234%2C004-0235%2C004-0236%2C004-0237%2C004-0238%2C004-0239%2C004-0240%2C004-0241%2C004-0242%2C004-0243%2C004-0244%2C004-0245%2C004-0246&p=1-All#all>
# 1. land use - land in crops, summerffallow land, seeded pasture, natural land, woodland/wetland, other
# 2. farm type - cattle ranching, beef, pork, poultry, sheep, goat, veg, grain, nursery, fruits
# 3. fruits n nuts - # of farms, acres of coverage
# 4. bees - bees on census day, # of farms and total bees
# 5. land practices & land features - winter grazing/feeding, rotational grazing, plowing green crops, winter cover crops, windbreaks

# Each of these datasets come with a .csv file for that section of the census data and a metadata .csv file

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
install.packages("raster")
library("raster")
library("sp")
library("sf")
install.packages("sjmisc")
library("sjmisc")

# Import Files ------------------------------------------------------------
farm.type <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/farm type_32100403/farm type_32100403.csv")
# Classification code = type of farm
land.use <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/land use_32100406/land use_32100406.csv")
fruits.n.nuts <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/fruits n nuts_32100417/fruits n nuts_32100417.csv")
bees <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/bees_32100432/bees_32100432.csv")
land.practice <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/land practices and features_32100411/land practices_32100411.csv")


# Download CanCensus Spatial Data -----------------------------------------
install.packages("cancensus")
library(cancensus)
options(cancensus.api_key="CensusMapper_02c70bf223570cf6f5dec048e9ee7e14")
cancensus.cache_path="canc"
install.packages("geojsonsf")
library(geojsonsf)
library(tidyverse)
library("sf")

# To view available Census datasets
# list_census_datasets()

# To view available named regions at different levels of Census hierarchy for the 2016 Census (for example)
# list_census_regions("CA16") # Row 4, number 59 is BC

# To view available Census variables for the 2016 Census
# list_census_vectors("CA16","region"=="59")
# bc.census.data<-list_census_vectors("CA16","region"=="59")

# I went with the below route to see if that works better instead:
# Read in the .shp for Canada census divisions (CD) data:
cen.divs.shp <-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/census divisions_can/lcd_000b16a_e.shp")

# I found the .shp for Canada Ag Regions (CAR)!! Read in Below:
can.ag.regions.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN census Ag Regions/lcar000b16a_e.shp")

# I found the .shp for Consolidated Census Subdivisions (CCS)!! Read in Below:
can.ccs.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN census cons subdivisions (CCS)/lccs000b16a_e.shp")


# Make sure CD is an sf object
cd.sf<- as(cen.divs.shp, "sf")
unique(cd.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"
# Filter down CD to only British Columbia
bc.cen.divs<-cd.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()

# Make sf and filter down to only British Columbia for Ag Regions (CAR):
can.ag.reg.sf<- as(can.ag.regions.shp, "sf")
unique(can.ag.reg.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"
bc.ag.regs<-can.ag.reg.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()

# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(can.ccs.shp, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"
bc.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()

# Spatialize the Ag Files -------------------------------------------------

# Filter the Ag Files down to just BC districts:
# See here: https://www.statology.org/filter-rows-that-contain-string-dplyr/  searched: 'Return rows with partial string, filter dplyr'
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 
land.use.bc <- land.use %>% filter(grepl("British Columbia", GEO)) 
fruits.nuts.bc <- fruits.n.nuts %>% filter(grepl("British Columbia", GEO)) 
bees.bc <- bees %>% filter(grepl("British Columbia", GEO)) 
land.prac.bc <- land.practice %>% filter(grepl("British Columbia", GEO)) 
write.csv(farm.type.bc, "farm_type_bc.csv")


# Dividing the Farm.Type Geocode into Seperate Columns & .shp -------------------------------------
# Dividing the Geocode into 3 Columns -------------------------------------
# Need to split the GEO column field into 3 columns: place name, province, and geocode
# Also need to drop the brackets from the geocode using 'gsub'
install.packages("stringi")
install.packages("stringr")
library("stringi")
library("stringr")
?gsub

# Trying this with base R, seperating based on a space (if possible)
# data.frame(do.call("rbind", strsplit(as.character(data$x), "-", fixed = TRUE)))
data.frame(do.call("rbind", strsplit(as.character(farm.type.bc$GEO), ",", fixed = TRUE)))
# Yikes, this divided all of them by anything with a space between it

# Try this with stringr:
#str_split_fixed(data$x, "-", 2)
str_split_fixed(farm.type.bc, ",", 2)

# Filtering to just the BC regions with a CARxxx number:
bc.farm.filter<-farm.type.bc %>%
  filter(., grepl("*CAR59*", farm.type.bc$GEO))
write_csv(bc.farm.filter, "bc_farms_filtered.csv")

unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.farm.2016<-bc.farm.filter %>%
  filter(., grepl("2016", bc.farm.filter$REF_DATE)) # Now there are 344 observations

# SUCCESS:Seperated out the CARID geocode into column: https://www.datasciencemadesimple.com/substring-function-in-r/
# https://statisticsglobe.com/split-data-frame-variable-into-multiple-columns-in-r
unique(bc.farm.2016$GEO) # There are 7 CAR codes here
library(stringr)

# bc.farm.2016$CARID<- str_sub(bc.farm.2016$GEO,42,58) to get a range of characters
bc.farm.2016$CARID<- str_sub(bc.farm.2016$GEO,-14) # YAY! This gives us 14 spots from the end
bc.farm.2016$CARID<- str_sub(bc.farm.2016$GEO,-13,-2) #This drops the brackets for us, sweet!

# Now lets chop this up to match the bc.ag.regs CARUID column:
bc.farm.2016$CARUID<- str_sub(bc.farm.2016$GEO,-10,-7) #This gives us the 4 digit code we need. DOPE!

unique(bc.farm.2016$CARUID) # WAHOO! That's 8 region ID's with 4 digits

# Join the BC Ag Regions with Ag Files:
farm.type.join <- merge(bc.ag.regs, bc.farm.2016, by.x = "CARUID", by.y = "CARUID")
# Got this to go through, now to view it!
str(farm.type.join)
plot_sf(farm.type.join)
st_write(farm.type.join,"farm_type_bc.shp")

# Land Use Join to .shp ---------------------------------------------------
# Now we do the same as with farm.type:
library("stringi")
library("stringr")

# Filtering to just the BC regions with a CARxxx number:
bc.land.use.filter<-land.use.bc %>%
  filter(., grepl("*CAR59*", land.use.bc$GEO))
write_csv(bc.land.use.filter, "bc_land_use_filtered.csv")

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.land.use.2016<-bc.land.use.filter %>%
  filter(., grepl("2016", bc.land.use.filter$REF_DATE)) # Now there are 344 observations

# SUCCESS:Seperated out the CARID geocode into column: https://www.datasciencemadesimple.com/substring-function-in-r/
unique(bc.land.use.2016$GEO) # There are 8 CAR codes here
library(stringr)

# bc.farm.2016$CARID<- str_sub(bc.farm.2016$GEO,42,58) to get a range of characters
bc.land.use.2016$CARID<- str_sub(bc.land.use.2016$GEO,-13,-2) #This drops the brackets for us, sweet!

# Now lets chop this up to match the bc.ag.regs CARUID column:
bc.land.use.2016$CARUID<- str_sub(bc.land.use.2016$GEO,-10,-7) #This gives us the 4 digit code we need. DOPE!

unique(bc.land.use.2016$CARUID) # WAHOO! That's 8 region ID's with 4 digits

# Join the BC Ag Regions with Ag Files:
land.use.join <- merge(bc.ag.regs, bc.land.use.2016, by.x = "CARUID", by.y = "CARUID")
# Got this to go through, now to view it!
str(land.use.join)
st_write(land.use.join,"land_use_bc.shp")

# Land Practices Join to .shp ---------------------------------------------------
# Now we do the same as with above:
library("stringi")
library("stringr")

# Filtering to just the BC regions with a CARxxx number:
bc.land.prac.filter<-land.prac.bc %>%
  filter(., grepl("*CAR59*", land.prac.bc$GEO))
write_csv(bc.land.prac.filter, "bc_land_prac_filtered.csv")

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.land.prac.2016<-bc.land.prac.filter %>%
  filter(., grepl("2016", bc.land.prac.filter$REF_DATE)) # Now there are 344 observations

# SUCCESS:Seperated out the CARID geocode into column: https://www.datasciencemadesimple.com/substring-function-in-r/
unique(bc.land.prac.2016$GEO) # There are 8 CAR codes here
library(stringr)

# bc.farm.2016$CARID<- str_sub(bc.farm.2016$GEO,42,58) to get a range of characters
bc.land.prac.2016$CARID<- str_sub(bc.land.prac.2016$GEO,-13,-2) #This drops the brackets for us, sweet!

# Now lets chop this up to match the bc.ag.regs CARUID column:
bc.land.prac.2016$CARUID<- str_sub(bc.land.prac.2016$GEO,-10,-7) #This gives us the 4 digit code we need. DOPE!

unique(bc.land.prac.2016$CARUID) # WAHOO! That's 8 region ID's with 4 digits

# Join the BC Ag Regions with Ag Files:
land.prac.join <- merge(bc.ag.regs, bc.land.prac.2016, by.x = "CARUID", by.y = "CARUID")
# Got this to go through, now to view it!
str(land.prac.join)
st_write(land.prac.join,"land_prac_bc.shp")

# Bees Join to .shp ---------------------------------------------------
# Now we do the same as with above:
library("stringi")
library("stringr")

# Filtering to just the BC regions with a CARxxx number:
bc.bees.filter<-bees.bc %>%
  filter(., grepl("*CAR59*", bees.bc$GEO))
write_csv(bc.bees.filter, "bc_bees_filtered.csv")

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.bees.2016<-bc.bees.filter %>%
  filter(., grepl("2016", bc.bees.filter$REF_DATE)) # Now there are 344 observations

# SUCCESS:Seperated out the CARID geocode into column: https://www.datasciencemadesimple.com/substring-function-in-r/
unique(bc.bees.2016$GEO) # There are 8 CAR codes here
library(stringr)

# bc.farm.2016$CARID<- str_sub(bc.farm.2016$GEO,42,58) to get a range of characters
bc.bees.2016$CARID<- str_sub(bc.bees.2016$GEO,-13,-2) #This drops the brackets for us, sweet!

# Now lets chop this up to match the bc.ag.regs CARUID column:
bc.bees.2016$CARUID<- str_sub(bc.bees.2016$GEO,-10,-7) #This gives us the 4 digit code we need. DOPE!

unique(bc.bees.2016$CARUID) # WAHOO! That's 8 region ID's with 4 digits

# Join the BC Ag Regions with Ag Files:
bees.join <- merge(bc.ag.regs, bc.bees.2016, by.x = "CARUID", by.y = "CARUID")
# Got this to go through, now to view it!
str(bees.join)
st_write(bees.join,"bees_bc.shp")


# Filter for CCS and merge with geometry ----------------------------------

# Filtering to just the BC regions with a CCS number:
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))
write_csv(bc.farm.filter.ccs, "bc_farms_filtered.ccs.csv")

bc.farm.filter.ccs.a<-farm.type.bc[grepl("*CCS59*", farm.type.bc$GEO), ]
str(bc.farm.filter.ccs.a)

unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., grepl("2016", bc.farm.filter.ccs$REF_DATE)) # Now there are 344 observations

# SUCCESS:Seperated out the CCS geocode into column: https://www.datasciencemadesimple.com/substring-function-in-r/
# https://statisticsglobe.com/split-data-frame-variable-into-multiple-columns-in-r
unique(bc.farm.2016.ccs$GEO) # There are 132 CCS codes here
library(stringr)

# Now lets chop this up to match the bc.ccs CCSUID column:
# This is causing issues because the farm CCS has an extra zero in the middle - now we need to chop both up and join
bc.farm.2016.ccs$CCSUID.crop<- str_sub(bc.farm.2016.ccs$GEO,-6,-2) #This gives usa 6 digit code - there is an extra zero!!
# Need to figure out how to remove a zero from middle of a string of numbers in a column

unique(bc.farm.2016.ccs$CCSUID.crop) # WAHOO! That's 8 region ID's with 6 digits

bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # There we go, now we have a matching 6 digits
unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
str(bc.farm.2016.ccs)
str(bc.farm.filter)
# Join the BC CCS with Ag Files:
farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 
unique(farm.ccs.join$GEO)

str(bc.ccs) # Classes sf and data.table
str(bc.farm.2016.ccs)
# Got this to go through, now to view it!
str(farm.ccs.join)
farm.ccs.j.sf<-as(farm.ccs.join, "sf")
plot_sf(farm.ccs.join)
st_write(farm.ccs.join,"farm_type_ccs.shp")
write_csv(farm.ccs.join, "farm_type_ccs.csv")
# There is something funky with this table.. ?

# Selecting Dominant Ag Types By Region -----------------------------------
# Going to pull out the top 2 farm count values for each of the CCS codes 
# https://statisticsglobe.com/select-row-with-maximum-or-minimum-value-in-each-group-in-r
library(dplyr)
number.of.ccs.farms<- farm.ccs.join$VALUE
ccs.groups<- farm.ccs.join$GEO

farm.ccs.join$VALUE<- as.integer(farm.ccs.join$VALUE)
# Pull out the top two values from the join:
top.two.ccs.farm.types<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE)  # YAY! This worked!
str(top.two.ccs.farm.types)

# Pull out the top one (total farms) from the join:
totals.ccs.farm.types<- farm.ccs.join %>% group_by(GEO) %>% top_n(1,VALUE)  # YAY! This worked!
str(totals.ccs.farm.types) # This seems to have given us what we need

write_csv(top.two.ccs.farm.types, "Dominant and Total Farm Types CCS.csv")
st_write(top.two.ccs.farm.types,"Dominant and Total Farm Types CCS.shp")

st_write(totals.ccs.farm.types, "Total Farms By Type CCS.shp") #These map well!

# Next up: need to figure out how to assign max values to individual colors and map (not sure how to do this)

# Trying to Get this to Plot - Doesn't want to bc it is a data.frame
plot(st_geometry(top.two.ccs.farm.types))
plot(farm.ccs.join["VALUE"])
as(top.two.ccs.farm.types, "sf") # This won't convert a data.frame or tibble 



