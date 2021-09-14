# Crown Lands & Postal Codes ----------------------------------------------
# This script is an attempt to overlay the postal zipcodes of British Columbia with
# the polygons representing the BC Crown Lands (public lands) and then to calculate
# what proportion of zipcodes from responses fall onto Crown Lands


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
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
# Need to figure out which zip code each intersect belongs to, then you can sum the area of the intersection within each zipcode:
 
# Let's do the intersection:
# bc.crown.zips.intersect <- st_intersection(bc.zips.valid, crown.valid) this is where the zips intersect crown land (we need to flip)
head(bc.crown.zips.intersect) # This returns a whole sfc ...
plot(st_geometry(bc.crown.zips.intersect)) # These are indeed our overlap areas!
bc.crown.zips.intersect <- st_intersection(crown.valid, bc.zips.valid) # This should be where crown lands intersect the zips

# Calculate the area of each intersection:
bc.crown.zips.intersect$intersection_area <-st_area(bc.crown.zips.intersect)

# use a spatial join to assign a zip code to each intersection
bc.crown.zips.join <- st_join(bc.crown.zips.intersect, left = FALSE, bc.zips.valid["zip"]) # join points
head(bc.crown.zips.join) # we successfully assigned zips to the intersection area

# use group_by %>% summarise() (use the help files to get the syntax) to get a zip code by zip code estimate of the 
# sum of the intersected areas
names(bc.crown.zips.join)[names(bc.crown.zips.join) == 'zip.y'] <- 'zip_intersects' #Just changed the column name here

sum_area_intersection <- aggregate(bc.crown.zips.join$intersection_area, by=list(zip_intersects=bc.crown.zips.join$zip_intersects), FUN=sum)
# This returns a column with summary of intersection area for each different zipcode
str(sum_area_intersection) #This is a df but needs to be joined so it has geometries?

# Let's see if we can join the sum_area column to the big table: https://github.com/tidyverse/dplyr/issues/2833
bc.crown.zips.join.sum.area <- sum_area_intersection %>% left_join(bc.crown.zips.join, by = "zip_intersects") # - BUT THIS ISNT SPATIAL
names(bc.crown.zips.join.sum.area)[names(bc.crown.zips.join.sum.area) == 'x'] <- 'sum_area_intersections' #Just changed the column name here
str(bc.crown.zips.join.sum.area)
# Let's make intersection_area numeric so there arent units issues
bc.crown.zips.join.sum.area$intersection_area <- as.numeric(bc.crown.zips.join.sum.area$intersection_area)
str(bc.crown.zips.join.sum.area)
# Calculate the Proportion of Intersection Area / Zipcode Area:
# bc.crown.zips.intersect$proportionCrownLand <- st_area(bc.crown.zips.intersect)/st_area(bc.zips.valid)*100 #This gives us a %

# Let's try this with the new columns: divide intersection_area over area_sum_inttersection:
# bc.crown.zips.join$proportionCrownLand <- (bc.crown.zips.join$intersection_area / sum_area_intersection)*100
bc.crown.zips.join.sum.area$proportionCrownLand <- (bc.crown.zips.join.sum.area$intersection_area / bc.crown.zips.join.sum.area$sum_area_intersections)*100
head(bc.crown.zips.join.sum.area)
str(bc.crown.zips.join.sum.area)
# This has returned % values, looks to be correct.. none are >100%

# we need to do the proportion but keep a column with zip IDs:
proportionCrownLand <- (bc.crown.zips.join.sum.area$intersection_area / bc.crown.zips.join.sum.area$sum_area_intersections)*100

bc.crown.zips.prop.join <- st_join(bc.crown.zips.join, left = FALSE, proportionCrownLand) # this won't work because there is no zipcodes tied to the props

range(bc.crown.zips.join.sum.area$proportionCrownLand)
# [1] 2.624172e-08 1.000000e+02  This confirms, none are above 100% !!
plot(bc.crown.zips.join)

str(bc.crown.zips.join.sum.area) # Only thing is that this is a data.frame ... not sf ?
plot(st_geometry(bc.crown.zips.join.sum.area))
# Trying to change this to sf:
bc.crown.zips.join.sf <- st_as_sf(x = bc.crown.zips.join.sum.area,                         
                               coords = c("LONGITU", "LATITUD"),
                               crs = albers.crs)
str(bc.crown.zips.join.sf) #Sweet, this worked, but they're points? how to do polygons
plot(st_geometry(bc.crown.zips.join.sum.area))
bc.crown.zips.join.sf <- st_as_sf(x = bc.crown.zips.join.sum.area,                         
                               geometry = c("geometry"),
                               crs = albers.crs, agr = "constant") 
?st_as_sf
# STOPPED HERE: still trying to configure it from points to polygo --------


# %>% group_by(zip_intersects) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("POLYGON")
str(bc.crown.zips.join.sf) # Still points... gotta fix this next

plot(st_geometry(bc.crown.zips.join.sf$geometry))

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







