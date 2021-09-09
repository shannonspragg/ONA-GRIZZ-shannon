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

# Re-project the Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the Ag Census areas projected to match the bears data
zips.reproj <- st_transform(bc.zips, st_crs(albers.crs))
plot(st_geometry(zips.reproj))
crown.reproj <- st_transform(bc.crown.lands.shp, st_crs(albers.crs))
plot(st_geometry(crown.reproj))

#make sure your coordinate systems projections are equal.
 st_crs(zips.reproj) == st_crs(crown.reproj)
# This returns TRUE so we are good

# Need to start by making the crown and zip reprojected files "valid"
 crown.valid<-st_make_valid(crown.reproj)
 str(crown.valid)
 st_is_valid(zips.reproj)
 bc.zips.valid<- st_make_valid(zips.reproj)
 str(bc.zips.valid)
 
# Finding where the Crown Lands & Zipcodes Intersect ----------------------
#Trying to find the proportion of crown land in each postal code and assign that to each respondent
 #     https://github.com/r-spatial/sf/issues/1710
 #    https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

# Trying to find proportion of intersection: (Matt's code below)
# zipcode.df <- zipcode.df %>%
# mutate(propCrownLands = (st_area(st_intersection(zipcode.sf, crownland.sf))/st_area(zipcode.sf) *100)
# We need to split this  up to work
 
# Let's do the intersection:
bc.crown.zips.intersect <- st_intersection(bc.zips.valid, crown.valid)
head(bc.crown.zips.intersect) # This returns a whole sfc ...
plot(st_geometry(bc.crown.zips.intersect)) # These are indeed our overlap areas!

# Don't NEED to do this part...
intersect.area <- st_area(bc.crown.zips.intersect)
head(intersect.area) # This is in m^2
bc.zips.area <- st_area(bc.zips.valid)
head(bc.zips.area) # This is also in m^2

bc.crown.zips.intersect$propCrownLand <- st_area(intersect.area)/st_area(bc.zips.area)*100 #This is causing errors ?

# Calculate the Proportion of Intersection Area / Zipcode Area:
bc.crown.zips.intersect$proportionCrownLand <- st_area(bc.crown.zips.intersect)/st_area(bc.zips.valid)*100
# This gave us a new column, seems to have worked.. is technically m^2 / m^2

plot(st_geometry(bc.crown.zips.intersect))

# Write this as a .shp for later:
st_write(bc.crown.zips.intersect, "/Users/shannonspragg/ONA_GRIZZ/Proportion Crown Lands x Survey Zips/BC Crown Proportion Zipcode Intersection.shp")

# Spatial Intersection : NOT what we wanted.. but still worked to show us visual overlaps  --------
intersect_full<- st_intersection(st_geometry(crown.valid), st_geometry(bc.zips.valid))
# Now this seemed to work... lets check it
intersect_full_tibble<- as_tibble(intersect_full) # This makes it a tibble https://rpubs.com/rural_gis/255550

plot(st_geometry(intersect_full))
# I THINK THIS WORKED?!
plot(st_geometry(bc.zips.valid)) #This plots our zipcode polygons
plot(st_geometry(crown.valid)) # This plots our crown land polygons

# Trying to Plot these on top of each other, with color assigned to overlap:
plot(st_geometry(crown.valid), col= 'grey', border='grey')
plot(st_geometry(intersect_full), col= 'red', add=TRUE)
# HECK YES, we can see the overlap areas in red!!







