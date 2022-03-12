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
PAs.soi.crop <- st_crop(bc.PAs.reproj, soi.bound.reproj)
plot(st_geometry(PAs.soi.crop))

metro.soi.crop <- st_crop(metro.reproj, soi.bound.reproj)
plot(st_geometry(metro.soi.crop))


# Rasterize our Points & Polygons: ----------------------------------------

# Make farm type a spatvector:
PA.soi.sv <- vect(PAs.soi.crop)

metro.soi.sv <- vect(metro.reproj)
plot(farm.count.sv)

# Now let's rasterize the farm type and count:
pas.soi.rast <- terra::rasterize(PA.soi.sv, biophys.cum.curmap, field = "")
plot(pa.soi.rast)

metro.soi.rast <- terra::rasterize(metro.soi.sv, biophys.cum.curmap, field = "")
plot(metro.soi.rast)

??terra::rasterize
