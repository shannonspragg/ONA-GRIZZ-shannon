# Overlaying WARP Points with CS Outputs ----------------------------------
# In this script I will be bringing in the 3 produced CS's (cum.resist, sociobio, grizzinc)
# and overlaying each individual one with the WARP points, buffering the points by 500m, and
# then extracting the attributes from each raster to each WARP point by location. The result should
# be the creation of three additional columns (one for each CS) in the master WARP df, representing these values.


# Load Packages -----------------------------------------------------------
library(sf)
library(raster)
library(tidyverse)
library(dplyr)
library(sp)
library(terra)
library(rgdal)


# Bring in WARP Master df and CS Rasters ----------------------------------
warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")
comb.resist.cum.curmap <- rast("/Users/shannonspragg/rasters/Combined Resistance_1/cum_currmap.tif")
sociobio.cum.curmap <- rast("/Users/shannonspragg/rasters/SocioBio Resistance CS/cum_currmap.tif")
grizzinc.cum.curmap <- rast("/Users/shannonspragg/rasters/Social GrizzIncrease CS/cum_currmap.tif")

combined.resist <- rast("/Users/shannonspragg/rasters/combined_resist.tif")
sociobio.resist <- rast("/Users/shannonspragg/rasters/sociobio_resist.tif")
survey.resist <- rast("/Users/shannonspragg/rasters/GrizzIncrease (Social)_2.tif")

# Bring in Provinces and Filter to BC -------------------------------------
can.prov <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN Province Boundaries/lpr_000b16a_e.shp")
bc.boundary <- can.prov %>% 
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>% 
  st_make_valid()
bc.reproj <- st_transform(bc.boundary, st_crs(warp.all.sp))
plot(st_geometry(bc.reproj))
st_write(bc.reproj, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Boundary.shp")


# Crop WARP Points to within the BC Boundary ------------------------------
warp.all.sp.bc <- st_crop(warp.all.sp, bc.reproj)
plot(st_geometry(warp.all.sp.bc)) # Cropped these to only the points in BC
st_crs(warp.all.sp.bc)
crs(warp.all.sp.bc)

# Check / Set CRS for Raster and Points -----------------------------------
 # Match the projection and CRS of the current map to the resistance map:
crs(comb.resist.cum.curmap) <- crs(combined.resist)

# Match the projection and CRS of the WARP to the resistance map:
st_crs(warp.all.sp.bc)

# Need to find out how to do this with an sf object 
crs(warp.all.sp.bc) 
crs(comb.resist.cum.curmap) # These don't match...

new.crs <- CRS("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs")
warp.reproj <- warp.all.sp.bc %>% st_transform(., crs(combined.resist))

plot(st_geometry(warp.reproj))
crs(warp.reproj)

plot(comb.resist.cum.curmap)
plot(st_geometry(warp.reproj, add=TRUE))

# Overlay WARP Points with CS Raster --------------------------------------
# Here I will extract the values from each raster to the points

# Need to make points a SpatVector:
warp.sv <- vect(warp.all.sp.bc)
st_crs(warp.all.sp.bc)
str(warp.sv)
crs(warp.sv)

plot(warp.sv)
warp.comb.resist.ext <- terra::extract(comb.resist.cum.curmap, vect(warp.all.sp.bc))

# Buffer the WARP Points AFTER OVERLAY--------------------------------------------------
# Here we buffer the WARP points by 500m before extracting the attributes from the current maps
warp.all.buf <- warp.all.sp.bc %>% 
  st_buffer(., 500)
plot(st_geometry(warp.all.buf)) # Check the buffers

st_crs(warp.all.buf)
