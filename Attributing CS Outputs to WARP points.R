# Overlaying WARP Points with CS Outputs ----------------------------------
# In this script I will be bringing in the produced CS's (grizzinc and biophysical)
# and overlaying each individual one with the WARP points, buffering the points by 500m, and
# then extracting the attributes from each raster to each WARP point by location. The result should
# be the creation of three additional columns (one for each CS) in the master WARP df, representing these values.

# UPDATE NOTE:
# No need to even use the "combined resistance" cs or resistance raster (this was solely to derive the grizz inc raster from).
# Should have: extracted values from BHS (not cs), a grizzinc CS (social), Biophysical CS (see biophys_norm_cum_current) in ONA folder, 
# and plain Social survey resistance values (not cs, extracted from normal raster).

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

# CS's for grizz increase and biophysical:
grizzinc.cum.curmap <- rast("/Users/shannonspragg/rasters/Social GrizzIncrease CS_1/cum_currmap.tif")
biophys.cum.curmap <- rast("/Users/shannonspragg/rasters/biophys_normalized_cum_currmap.tif")

# Just survey response layer (not CS):
survey.resist <- rast("/Users/shannonspragg/rasters/GrizzIncrease (Social)_2.tif")
# BHS layer:
grizz.dens <- rast("/Users/shannonspragg/ONA_GRIZZ/Grizz Density rasters/grizz_dens.tif")
plot(grizz.dens)

# Bring in Provinces and Filter to BC -------------------------------------
can.prov <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN Province Boundaries/lpr_000b16a_e.shp")
bc.boundary <- can.prov %>% 
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>% 
  st_make_valid()
bc.reproj <- st_transform(bc.boundary, st_crs(warp.all.sp))
plot(st_geometry(bc.reproj))
st_write(bc.reproj, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Boundary.shp")

# Read this in for future runs:
bc.reproj <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Boundary.shp")


# Crop WARP Points to within the BC Boundary ------------------------------
warp.all.sp.bc <- st_crop(warp.all.sp, bc.reproj)
plot(st_geometry(warp.all.sp.bc)) # Cropped these to only the points in BC
st_crs(warp.all.sp.bc)
crs(warp.all.sp.bc)

st_write(warp.all.sp.bc, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Sp Full Yr BC.shp")

# Read this in for future runs:
warp.all.sp.bc <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Sp Full Yr BC.shp")


# Check / Set CRS for Raster and Points -----------------------------------
 # Match the projection and CRS of the current map to the resistance maps:
 # Here we use the biophysical.cum.curmap to be the template crs

    # Bear Density (BHS) Current Map:
crs(grizz.dens) <- crs(biophys.cum.curmap) 
crs(biophys.cum.curmap) == crs(grizz.dens) # Nice, this worked --> now in BC Albers EPSG 3005
    # Survey Resistance Map:
crs(survey.resist) <- crs(biophys.cum.curmap) 
crs(survey.resist) == crs(biophys.cum.curmap) # Nice, this worked --> now in BC Albers EPSG 3005
    # GrizzInc Survey Current Map:
crs(grizzinc.cum.curmap) <- crs(biophys.cum.curmap) 
crs(grizzinc.cum.curmap) == crs(biophys.cum.curmap) # Nice, this worked --> now in BC Albers EPSG 3005
    
# Match the projection and CRS of the WARP to the resistance map:
st_crs(warp.all.sp.bc) # This is in NAD83 Conus Albers - EPSG 5070

# Match the sf points CRS directly to the resistance raster:
warp.reproj <- st_make_valid(warp.all.sp.bc) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
  
plot(st_geometry(warp.reproj))
st_crs(warp.reproj)
crs(biophys.cum.curmap) # The same as above, just formatted differently - success!

# Need to make points a SpatVector:
warp.sv <- vect(warp.reproj)
str(warp.sv)
crs(warp.sv)

# Plot them together to see if projection is same:
plot(biophys.cum.curmap)
plot(warp.sv, add = TRUE) # HECK YES

plot(grizz.dens)
plot(warp.sv, add = TRUE) # Sweet

plot(survey.resist)
plot(warp.sv, add = TRUE) # Nice

plot(grizzinc.cum.curmap)
plot(warp.sv, add = TRUE) # AGAIN FOR THE PEOPLE IN THE BACK

# Buffer the WARP Points (Before Overlay) --------------------------------------------------
# Here we buffer the WARP points by 500m before extracting the attributes from the current maps
warp.all.buf <- warp.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(warp.all.buf)) # Check the buffers

st_crs(warp.all.buf)

# Let's Turn the Buffered Points into a SpatVector:
warp.sv.buf <- vect(warp.all.buf)
crs(warp.sv.buf)
str(warp.sv.buf)
plot(warp.sv.buf)

# Overlay WARP Points with CS Raster BUFFERED --------------------------------------
# Here I will extract the mean values from each raster to the buffered points

warp.biophys.b.ext <- terra::extract(biophys.cum.curmap, warp.sv.buf, mean, na.rm = TRUE) 
# This gives us the mean value of each buffered area --> what we want!
warp.social.resist.b.ext <- terra::extract(survey.resist, warp.sv.buf, mean, na.rm = TRUE) 
warp.grizzinc.b.ext <- terra::extract(grizzinc.cum.curmap, warp.sv.buf, mean, na.rm = TRUE) 
warp.bhs.b.extract <- terra::extract(grizz.dens, warp.sv.buf, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
warp.reproj$BiophysExtract <- warp.biophys.b.ext[,2]
warp.reproj$SurveyResistExtract <- warp.social.resist.b.ext[,2]
warp.reproj$GrizzIncExtract <- warp.grizzinc.b.ext[,2]
warp.reproj$BHSExtract <- warp.bhs.b.extract[,2]


# Overlay WARP Points with CS Raster UNBUFFERED --------------------------------------
# Here I will extract the values from each raster to the points

warp.comb.resist.ext <- terra::extract(comb.resist.cum.curmap, warp.sv) # YAY! This worked
warp.sociobio.ext <- terra::extract(sociobio.cum.curmap, warp.sv) # YAY! This worked
warp.grizzinc.ext <- terra::extract(grizzinc.cum.curmap, warp.sv) # YAY! This worked

# Create New Column(s) for Extracted Values:
warp.reproj$CombResistExtract <- warp.comb.resist.ext[,2]
warp.reproj$SociobioExtract <- warp.sociobio.ext[,2]
warp.reproj$GrizzIncExtract <- warp.grizzinc.ext[,2]





# Create & Save New Master DF ------------------------------------------------------
# Now we should have a "master dataframe with these values, in the same projection:
warp.master.complete <- warp.reproj
str(warp.master.complete)
st_crs(warp.master.complete)
head(warp.master.complete)

# Let's save this to our folder as a new dataframe .shp:
st_write(warp.master.complete, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Master DF (+CS resistance values)/WARP Master DF (+CS resist values).shp")

