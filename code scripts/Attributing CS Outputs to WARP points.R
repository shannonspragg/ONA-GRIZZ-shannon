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


# Crop WARP Points to within the BC Boundary (skip this) ------------------------------
plot(st_geometry(warp.all.sp))
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
st_crs(warp.all.sp) # This is in NAD83 BC Albers - EPSG 3005

# Match the sf points CRS directly to the resistance raster:
warp.reproj <- st_make_valid(warp.all.sp) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
  
plot(st_geometry(warp.reproj))
st_crs(warp.reproj)
crs(biophys.cum.curmap) # The same as above, just formatted differently - success!

# Check Raster Resolutions:
str(grizz.dens)
res(biophys.cum.curmap)
res(survey.resist)
res(grizzinc.cum.curmap)

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


# Save this as new file ---------------------------------------------------

st_write(warp.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /updated.master.df.shp")
warp.reproj <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /updated.master.df.shp")

# Check for NA's: ---------------------------------------------------------
which(is.na(warp.reproj$BphysEx)) # We have about 70 NA's here..
which(is.na(warp.reproj$SrvyRsE)) # NO NA's
which(is.na(warp.reproj$GrzzInE)) # NO NA's
which(is.na(warp.reproj$BHSExtr)) # We have about 57 NA's here..

# NEED TO: figure out the cause of these different sets of NA's in Biophys and BHS...

# Let's try to plot these NA's and check their location:
# Plot the first NA:
plot(biophys.cum.curmap)
plot(st_geometry(warp.all.sp[30031,]), col = "red", add = TRUE)
# This somehow excludes the point value --> gives NA
plot(survey.resist)
plot(st_geometry(warp.all.sp[30031,]), col = "red", add = TRUE)
# This includes the point value --> no NA

plot(grizz.dens)
plot(st_geometry(warp.all.sp[14968,]), col = "red", add = TRUE)
warp.reproj[14968,]
warp.reproj[8108,]


# Drop the NA Rows: -------------------------------------------------------
  # To make things easier, we are just going to remove the rows that contain NA values for both the boiphysical
  # raster and the BHS raster - should be about 70 rows removed

droprecords <- warp.reproj %>% drop_na(BphysEx)
warp.dropped <- droprecords %>% drop_na(BHSExtr)

# Check to see if the NA's we wanted removed are gone:
which(is.na(warp.dropped$BphysEx)) # Nice, this removed the NA's
which(is.na(warp.dropped$BHSExtr)) # This cleared the NA's
which(is.na(warp.dropped$SrvyRsE))
which(is.na(warp.dropped$GrzzInE)) # NO NA's
which(is.na(warp.dropped$Dm_Fr_T)) # NO NA's
which(is.na(warp.dropped$Ttl_F_C)) # NO NA's

# Nice, now we are 69 rows fewer - not bad

# Replace NA Values with Zero ---------------------------------------------
  # It seems like all the NA values on those two rasters result from a point being right on the edge, where
  # there isn't a current value to begin with. So if we replace them with zeros, I don't think this is a big deal.
# warp.reproj.no.na <- mutate_at(warp.reproj, c("BiophysExtract", "BHSExtract"), ~replace(., is.na(.), 0))

which(is.na(warp.reproj.no.na$BiophysExtract)) # This made them all 0
which(is.na(warp.reproj.no.na$BHSExtract)) # Same as above

# Make sure all of our covariate columns have 0 NA's : CHECK!

which(is.na(warp.reproj.no.na$ds__PA_)) 
which(is.na(warp.reproj.no.na$dstn___)) 
which(is.na(warp.reproj.no.na$Dm_Fr_T))
which(is.na(warp.reproj.no.na$Ttl_F_C))
which(is.na(warp.reproj.no.na$BiophysExtract))
which(is.na(warp.reproj.no.na$SurveyResistExtract))
which(is.na(warp.reproj.no.na$GrizzIncExtract))
which(is.na(warp.reproj.no.na$BHSExtract)) 
# Nice, all of the above are good to go
# NOTE: attractant column has some NA's, but that is only one in the DF

# st_write(warp.reproj.no.na, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Sp Full - Produced/warp.master.na.rem.shp")



# Create & Save New Master DF ------------------------------------------------------
# Now we should have a "master dataframe with these values, in the same projection:
warp.master.complete <- warp.dropped
str(warp.master.complete)
st_crs(warp.master.complete)
head(warp.master.complete)

# Let's save this to our folder as a new dataframe .shp:
st_write(warp.master.complete, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Master DF (+CS resistance values)/WARP Master DF (+CS resist values).shp")
warp.master.corrected <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Master DF (+CS resistance values)/WARP Master DF (+CS resist values).shp")
