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

# Check the projections:
st_geometry(bc.zips) #This is WGS84 CRS

st_crs(bc.crown.lands.shp) # This is NAD83 and BC Albers

# Lets  try matching the zips CRS to the crown land CRS:
zips.reproj <- st_transform(bc.zips, st_crs(bc.crown.lands.shp))
st_crs(zips.reproj)

#make sure your coordinate systems projections are equal.
 st_crs(zips.reproj) == st_crs(bc.crown.lands.shp)
# This returns TRUE so we are good

# Finding where the Crown Lands & Zipcodes Intersect ----------------------
#Trying to find the proportion of crown land in each postal code and assign that to each respondent
 #     https://github.com/r-spatial/sf/issues/1710
 #    https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

# Need to start by making the crown and zip reprojected files "valid"
crown.valid<-st_make_valid(bc.crown.lands.shp)
 str(crown.valid)
 st_is_valid(zips.reproj)
 bc.zips.valid<- st_make_valid(zips.reproj)
 str(bc.zips.valid)
 
# Lets try this on a smaller chunk of the zipcodes data to see if it works ok:
zips.mini<- bc.zips.valid[1:5, ]
# crown.mini <- crown.reproj[1:5, ] # Don't need this at all
str(zips.mini)
st_crs(zips.mini)
intersect_mini <- st_intersection(crown.valid, zips.mini) 
# This is returning an error: attribute variables are assumed to be spatially constant throughout all geometries 

# The mini data kept getting errors, but running the full might work:

intersect_full<- st_intersection(st_geometry(crown.valid), st_geometry(bc.zips.valid))
# Now this seemed to work... lets check it
intersect_full_tibble<- as_tibble(intersect_full) # This makes it a tibble

plot(st_geometry(intersect_full))
# I THINK THIS WORKED?!
plot(st_geometry(bc.zips.valid)) #This plots our zipcode polygons
plot(st_geometry(crown.valid)) # This plots our crown land polygons

# Trying to Plot these on top of each other, with color assigned to overlap:
plot(st_geometry(crown.valid), col= 'grey', border='grey')
plot(st_geometry(intersect_full), col= 'red', add=TRUE)
# HECK YES, we can see the overlap areas in red!!

# Calculate the Area of Overlapping Zipcode & Crown Lands -----------------
intersect_full_tibble$area_overlap <- st_area(intersect_full_tibble$geometry)
#This seems to work with the tibble.. not sure about the sfc_geometry

# Is there a way to attach this to the broader intersect_full object?


# Calculating the Proportion of Crown Land in Survey Zipcodes -------------
# Not sure how to do this yet



#intersect_bcz <- st_intersection(st_geometry(crown.valid), st_geometry(bc.zips.valid))  %>%
# dplyr::mutate(intersect_area = st_area(.)) #%>%   # create new column with shape area  
#  dplyr::select(NAME, intersect_area)   # only select columns needed to merge






