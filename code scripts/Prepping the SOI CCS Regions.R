
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
warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10km_buf.shp")

# Our WARP df with pres abs points:
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_complete.shp")

soi.ccs.crop <- st_read( "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp") 

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


# Buffer our SOI Region by 10km to match WARP df:(skip this now) -------------------------

# Buffer our Boundary to Match Points:
soi.10k.buf <- st_buffer(soi.region, 10000)

st_write(soi.10k.buf, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")


# Crop CCS Down to SOI 10km Extent (skip): --------------------------------------------
soi.ccs.crop <- st_intersection(bc.ccs.reproj, soi.10k.buf)

# Write this as a .shp for later (skip): -----------------------------------------

st_write(soi.ccs.crop, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")
soi.ccs.crop <- st_read( "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

# Assign the WARP Points to a CCS Region: ---------------------------------
### Here we want to overlay the points with the regions, adding a column in the warp data that is CCS region ID, 
#   make sure this is a factor, to fit this as a varying intercept

# Check to see if our projections match:
st_crs(warp.pres.abs) == st_crs(soi.ccs.crop) # [TRUE] = These ARE the same

# Plot these together:
plot(st_geometry(soi.crop))
plot(st_geometry(warp.df), add=TRUE)

# Assign our points to a CCS category:
warp.ccs.join <- st_join(warp.pres.abs, left = TRUE, soi.ccs.crop) # join points
#warp.ccs.join.2 <- st_join(warp.df, soi.ccs, join= st_within)

head(warp.ccs.join) # HECK TO THE YES - we successfully assigned points to a farm type category

# Delete the columns we don't want:
warp.ccs.join$PRNAME <- NULL
warp.ccs.join$PRNTCDVSNC <- NULL
warp.ccs.join$PRUID <- NULL
warp.ccs.join$CDUID <- NULL
warp.ccs.join$CPRVNCCD <- NULL
warp.ccs.join$FTRCD <- NULL
warp.ccs.join$CPRVNCCD <- NULL
warp.ccs.join$PRNTCDVSNC <- NULL
warp.ccs.join$FFCTVDT <- NULL
warp.ccs.join$XPRDT <- NULL
warp.ccs.join$OBJECTID <- NULL
warp.ccs.join$AREA_SQM <- NULL
warp.ccs.join$FEAT_LEN <- NULL 
warp.ccs.join$CDNAME <- NULL
warp.ccs.join$CDTYPE <- NULL
warp.ccs.join$CPRVNCNM <- NULL
warp.ccs.join$CCSNAME.x <- NULL
warp.ccs.join$CCSUID.x <- NULL

# Rename these quick:
names(warp.ccs.join)[names(warp.ccs.join) == "CCSUID.y"] <- "CCSUID"
names(warp.ccs.join)[names(warp.ccs.join) == "CCSNAME.y"] <- "CCSNAME"

head(warp.ccs.join)

# Check for NA's!!! -------------------------------------------------------

which(is.na(warp.ccs.join$CCSNAME)) # no NAs
which(is.na(warp.ccs.join$CCSUID)) # yay!!



# Save this new df for future purposes:
st_write(warp.ccs.join, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_final.shp")

