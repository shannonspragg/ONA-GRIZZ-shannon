# Create Distance to PA and Metro Variable Rasters: ------------------------------
## Need to use the terra::distance function to take a raster of the SOI PA's and Metro Area's and a spatvector of the pseudo-abs points 
# and calculate the distance for all cell values.


# Load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
#install.packages("fasterize")
library(fasterize)
library(terra)
library(stars)


# Bring in Data: ----------------------------------------------------------

# Our pres-abs dataframe:
warp.pres.abs.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_complete.shp")

# BC PA's (need to crop down to SOI):
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")
str(bc.PAs) # Proper sf object, nice

# BC Metro Areas (also crop down to SOI):
bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census metro areas/CEN_CENSUS_METRO_AREAS_SVW/CNCNSSMTRR_polygon.shp")
str(bc.metro)

# SOI 10km Boundary:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")

# Bring in one of our rasters for rasterizing polygon data later:
biophys.cum.curmap <- rast("/Users/shannonspragg/rasters/biophys_normalized_cum_currmap.tif")


# Reproject All Data ------------------------------------------------------
# Now we have the protected areas projected to match the biophys raster:
warp.pa.reproj <- st_make_valid(warp.pres.abs.df) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
soi.bound.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(biophys.cum.curmap))


# Check to see if they match:
st_crs(warp.pa.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(soi.bound.reproj) # [TRUE]


# Crop PA's & Metro to SOI Region: ----------------------------------------
# Crop these points to just BC:
PAs.soi.crop <- st_intersection(bc.PAs.reproj, soi.bound.reproj)
plot(st_geometry(PAs.soi.crop))

metro.soi.crop <- st_intersection(metro.reproj, soi.bound.reproj)
plot(st_geometry(metro.soi.crop))
plot(st_geometry(soi.bound.reproj), add=TRUE) # This works

# Rasterize our Points & Polygons: ----------------------------------------

# Make our data spatvectors:
PA.soi.sv <- vect(PAs.soi.crop)

metro.soi.sv <- vect(metro.soi.crop)
plot(metro.soi.sv)

warp.ps.sv <- vect(warp.pa.reproj)


# Calculate Distance to PA's & Metro from points: -------------------------

# Need to use our spatvectors:

dist.pa.matrix <- terra::distance(warp.ps.sv, PA.soi.sv)

dist.met.matrix <- terra::distance(warp.ps.sv, metro.soi.sv)

# Add Distance Variable into Data table

warp.ps.sv$dist2PA<-dist.pa.matrix
head(warp.ps.sv)

warp.ps.sv$dist2Metro <- dist.met.matrix

# Remove the units from the values (note: in meters)
as.numeric(warp.ps.sv$dist2PA)
as.numeric(warp.ps.sv$dist2Metro)

# Convert units from meters to km:
library(measurements)
warp.ps.sv$dist2PA<-conv_unit(warp.ps.sv$dist2PA,"m","km")

warp.ps.sv$dist2Metro <- conv_unit(warp.ps.sv$dist2Metro, "m", "km")

head(warp.ps.sv) # Perfect!


# Make Metro and PA Dist Rasters: -----------------------------------------

pa.soi.rast <- terra::rasterize(warp.ps.sv, biophys.cum.curmap, field = "dist2PA")

metro.soi.rast <- terra::rasterize(warp.ps.sv, biophys.cum.curmap, field = "dist2Metro")

# Check this to see if it looks right:
plot(warp.ps.sv)
plot(pa.soi.rast, add=TRUE) #YASS


# Save our Rasters: -------------------------------------------------------
terra::writeRaster(pa.soi.rast, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dist2PA_raster.tif")
terra::writeRaster(metro.soi.rast, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dist2metro_raster.tif" )


