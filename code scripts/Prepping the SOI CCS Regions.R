
# Adding a Varying Intercept: ---------------------------------------------
  ###### Due to the low preformance of our posterior model on the predictive check, we look at adding 
# a varying intercept (i.e., some sort of category that we might imagine captures variation that your predictors aren't) 
# Using the CCS regions within our SOI ecoprovince may help for spatial autocorrelation within the region (but not between)


##################### Calculate How many CCS regions in SOI: ----------------------------------


# Bring in Packages: ------------------------------------------------------
library("rgdal")
library("sf")
library("sp")
library("raster")
library("tidyverse")
library("dplyr")


# Bring in Data: ----------------------------------------------------------

soi.region <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Ecoprovinces/south.interior.shp")
str(soi.region)

bc.ccs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census subdivs/BC CCS.shp")

# Re-project the Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the Ag Census areas projected to match the bears data
bc.ccs.reproj <- st_transform(bc.ccs, st_crs(soi.region))

# Check to see if they match:
st_crs(bc.ccs.reproj) == st_crs(soi.region) # [TRUE] = These ARE now the same

# Plot these together:
plot(st_geometry(bc.ccs.reproj))
plot(st_geometry(soi.region), add=TRUE)


# Crop CCS Down to SOI Extent: --------------------------------------------
soi.crop <- st_intersection(bc.ccs.reproj, soi.region)

# Write this as a .shp for later: -----------------------------------------

st_write(soi.crop, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS.shp")

