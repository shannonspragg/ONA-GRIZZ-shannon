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
bear.habitat.berry <- rast("/Users/shannonspragg/rasters/Huck_kcal_adjusted.tif")
biophys <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/biophys_normalized_cum_currmap.tif")


# Bring in Provinces and Filter to BC -------------------------------------
can.prov <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN Province Boundaries/lpr_000b16a_e.shp")
bc.boundary <- can.prov %>% 
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>% 
  st_make_valid()
bc.reproj <- st_transform(bc.boundary, st_crs(warp.all.sp))
plot(st_geometry(bc.reproj))
st_write(bc.reproj, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Boundary.shp")

bc.reproj <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Boundary.shp")

# Crop WARP Points to within the BC Boundary ------------------------------
warp.all.sp.bc <- st_crop(warp.all.sp, bc.reproj)
plot(st_geometry(warp.all.sp.bc)) # Cropped these to only the points in BC
st_crs(warp.all.sp.bc)
crs(warp.all.sp.bc)

st_write(warp.all.sp.bc, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Sp Full Yr BC.shp")

# Check / Set CRS for Raster and Points -----------------------------------
 # Match the projection and CRS of the current map to the resistance maps:
    # Combined Resistance Current Map:
crs(comb.resist.cum.curmap) <- crs(combined.resist) 
crs(comb.resist.cum.curmap) == crs(combined.resist) # Nice, this worked --> now in BC Albers EPSG 3005
    # Sociobio Current Map:
crs(sociobio.cum.curmap) <- crs(combined.resist) 
crs(sociobio.cum.curmap) == crs(combined.resist) # Nice, this worked --> now in BC Albers EPSG 3005
    # GrizzInc Survey Current Map:
crs(grizzinc.cum.curmap) <- crs(combined.resist) 
crs(grizzinc.cum.curmap) == crs(combined.resist) # Nice, this worked --> now in BC Albers EPSG 3005
    # CLayton's Huck Model (BHS):
crs(bear.habitat.berry) <- crs(combined.resist) 
crs(bear.habitat.berry) == crs(combined.resist) # Nice, this worked --> now in BC Albers EPSG 3005

# Match the projection and CRS of the WARP to the resistance map:
st_crs(warp.all.sp.bc) # This is in NAD83 Conus Albers - EPSG 5070

# Match the sf points CRS directly to the resistance raster:
warp.reproj <- st_make_valid(warp.all.sp.bc) %>% 
  st_transform(crs=crs(combined.resist))
  
plot(st_geometry(warp.reproj))
st_crs(warp.reproj)
crs(comb.resist.cum.curmap) # The same as above, just formatted differently - success!

# Need to make points a SpatVector:
warp.sv <- vect(warp.reproj)
st_crs(warp.all.sp.bc)
str(warp.sv)
crs(warp.sv)

# Plot them together to see if projection is same:
plot(comb.resist.cum.curmap)
plot(warp.sv, add = TRUE) # HECK YES

plot(sociobio.cum.curmap)
plot(warp.sv, add = TRUE) # GOT IT

plot(bear.habitat.berry)
plot(warp.sv, add = TRUE)

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

warp.comb.resist.b.ext <- terra::extract(comb.resist.cum.curmap, warp.sv.buf, mean, na.rm = TRUE) 
# This gives us the mean value of each buffered area --> what we want!
warp.sociobio.b.ext <- terra::extract(sociobio.cum.curmap, warp.sv.buf, mean, na.rm = TRUE) 
warp.grizzinc.b.ext <- terra::extract(grizzinc.cum.curmap, warp.sv.buf, mean, na.rm = TRUE) 
warp.bhs.b.extract <- terra::extract(bear.habitat.berry, warp.sv.buf, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
warp.reproj$CombResistExtract <- warp.comb.resist.b.ext[,2]
warp.reproj$SociobioExtract <- warp.sociobio.b.ext[,2]
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
plot(st_geometry(warp.master.complete))

# Let's save this to our folder as a new dataframe .shp:
st_write(warp.master.complete, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Master DF (+CS resistance values)/WARP Master DF (+CS resist values).shp")

