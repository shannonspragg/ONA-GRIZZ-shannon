# Creating a WARP All Species Master Dataframe ----------------------------
# This is going to be a longer script, but hopefully we can condense this efficently 
# given the cleaned up scripts this will be compiled from.
# The following code will be re-running the variables (dist to PA's, dist to Metro, 
# Farm Type, and Farm count) for the all-species full year data and then compiling that 
# into a full-species master df for the binomial regression


# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)

# Bring in the WARP All Speciec 1 Year Data -------------------------------
conflict.data.all<-read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP 3.24.20 to 3.31.21 full .csv")
head(conflict.data.all)

# Merge the two encounter columns into one total encounter column:
conflict.data.all$total_encounter<-conflict.data.all$encounter_adults + conflict.data.all$encounter_young
head(conflict.data.all)

# Convert selected species to 1's and all others to 0's:

bears.conflict.all<- conflict.data.all %>% 
  mutate(conflict.data.all, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
head(bears.conflict.all)

# Making Conflict Data a Spatial Dataframe:
bc.sp<-structure(bears.conflict.all,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.sp)
xy<-bc.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

bears.sf <- as(bears.spdf, "sf")
str(bears.sf) # This gives us a sf data frame, good!

# Bring in the PA,  Metro,  Ag Type,  and Farm Count Data -----------------
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")
str(bc.PAs) # Proper sf object, nice

bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census metro areas/CEN_CENSUS_METRO_AREAS_SVW/CNCNSSMTRR_polygon.shp")
str(bc.metro)

farm.type <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/farm type_32100403/farm type_32100403.csv")

bc.ccs<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census subdivs/BC CCS.shp")
str(bc.ccs)

# Reproject All Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the protected areas projected to match the bears data
bears.reproj <- st_transform(bears.sf, st_crs(albers.crs))
bc.PAs.reproj <- st_transform(bc.PAs, st_crs(albers.crs))
metro.reproj <- st_transform(bc.metro, st_crs(albers.crs))
bc.ccs.reproj <- st_transform(bc.ccs, st_crs(albers.crs))

# Check to see if they match:
st_crs(bears.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(bc.ccs.reproj) # [TRUE]

# Prep Variable 1: Dist to PA's -------------------------------------------
#Calculation of the distance between the PA's and our points
dist.pts2pas <- st_distance(bears.reproj, bc.PAs.reproj)
head(dist.pts2pas)

# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist <- apply(dist.pts2pas, 1, min)
str(min.dist)

# Add Distance Variable into Datatable:
bears.reproj$distance_to_PAs<-min.dist
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$distance_to_PAs)

# Convert units from meters to km:
install.packages("measurements")
library(measurements)

dist_in_km<-conv_unit(bears.reproj$distance_to_PAs,"m","km")
str(dist_in_km)
bears.reproj$distance_to_PA_km<-dist_in_km
bears.reproj$distance_to_PAs <- NULL
str(bears.reproj)

# Now bears.reproj has our Dist to PA column added in

# Prep Variable 2: Dist to Metro Areas ------------------------------------
#Calculation of the distance between the metro areas and our points

dist.pts2met <- st_distance(bears.reproj, metro.reproj)
head(dist.pts2met)

# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.met <- apply(dist.pts2met, 1, min)

# Add Distance Variable into Datatable
# Here we create a new column for Distance to Metro Areas in km
bears.reproj$distance_to_metro_km<-min.dist.met
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$distance_to_metro_km)

# Convert units from meters to km:
bears.reproj$distance_to_metro_km<-conv_unit(bears.reproj$distance_to_metro_km,"m","km")
head(bears.reproj)

# This added the dist to metro areas column to our bears.reproj df




# Prep Variable 3: the Dominant Ag Type by CCS ----------------------------
# Filter the Ag Files down to just BC districts:
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 
unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filtering to just the BC regions with a CCS number:
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))

# Filter for just the 2016 census results (this has 2011 and 2016):
bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., grepl("2016", bc.farm.filter.ccs$REF_DATE)) # Now there are 344 observations
head(bc.farm.2016.ccs)

# Crop the CCS column for bc.ccs:
bc.ccs.reproj$CCSUID.crop<- str_sub(bc.ccs.reproj$CCSUID,-5,-1) # There we go, now we have a matching 6 digits
unique(bc.ccs.reproj$CCSUID.crop) #This is a 5 digit code

# Do the same for the farm data:
bc.farm.2016.ccs$CCSUID.crop<- str_sub(bc.farm.2016.ccs$GEO,-6,-2) #This gives usa 6 digit code - there is an extra zero!!

# Join the BC CCS with Ag Files:
farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs.reproj, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 
head(farm.ccs.join)
str(farm.ccs.join) # Classes sf and data.table

# Extract Total Farm Counts by CCS: (Do this with the dominant farms too)
total.farms.bc<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE) %>% slice_min(., order_by = "VALUE")
# This successfully gives us total farm count by CCS region
dominant.farms.bc<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE) %>% slice_tail()
# YUSS! This gives us the dominant type WITHOUT the total farms :))

st_write(dominant.farms.bc,"Dominant Farm Types by CCS All Species.shp")

