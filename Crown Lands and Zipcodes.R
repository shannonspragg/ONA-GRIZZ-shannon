# Crown Lands & Postal Codes ----------------------------------------------
# This script is an attempt to overlay the postal zipcodes of British Columbia with
# the polygons representing the BC Crown Lands (public lands) and then to calculate
# what proportion of zipcodes from responses fall onto Crown Lands


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
install.packages("raster")
library("raster")
library("sp")
library("sf")

# Import the Crown Land and Zipcode .shp's ----------------------------------------------
# This is the .shp for Crown Land parcels across BC! Read in Below:
bc.crown.lands.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC crown reservations/TA_CROWN_RSRV_NOTATIONS_SVW/TA_CRN_SVW_polygon.shp")
plot(st_geometry(bc.crown.lands.shp))
st_geometry(bc.crown.lands.shp) #The CRS for this is not WGS84

# Now we bring in the survey postal zipcodes shp:
bc.zips<- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Survey Zipcodes/BC.survey.res.zips.sf.shannon.shp")
plot(st_geometry(bc.zips))

st_geometry(bc.zips) #This is WGS84 CRS

#Need to reproject the crown lands to match the survey data:
crown.reproj <- st_transform(bc.crown.lands.shp, st_crs(bc.zips))
st_crs(crown.reproj)

#make sure your coordinate systems projections are equal.
st_crs(crown.reproj) == st_crs(bc.zips)
# This returns TRUE so we are good

# Finding where the Crown Lands & Zipcodes Intersect ----------------------
#Trying to find the proportion of crown land in each postal code and assign that to each respondent

# st_is_valid(st_make_valid(st_set_precision(bc.zips, 1e6)))     https://github.com/r-spatial/sf/issues/1710
#This doesn't do what I want it to...
# sf_use_s2(FALSE)        https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
#This just turns off the spherical geometry
crown.valid<-st_make_valid(crown.reproj)
str(crown.valid)
bc.zips.valid<- st_make_valid(bc.zips)
str(bc.zips.valid)

??st_intersection
intersect_bcz <- st_intersection(crown.valid, bc.zips.valid)

intersect_bcz <- st_intersection(crown.valid, bc.zips.valid)  %>%
 dplyr::mutate(intersect_area = st_area(.)) %>%   # create new column with shape area  
  dplyr::select(NAME, intersect_area)   # only select columns needed to merge

# The "Evaluation error: Found 2 features with invalid spherical geometry" suggests that some of your polygon geometries 
# are invalid (possibly because they were digitized badly in the first place)





