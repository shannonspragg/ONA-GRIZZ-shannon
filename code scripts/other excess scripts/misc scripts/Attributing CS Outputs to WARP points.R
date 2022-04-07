# Overlaying WARP Points with CS Outputs ----------------------------------
  ## Here we bring in the predictor rasters and overlay each individual one with the WARP points, buffering the points by 500m, and
  # then extract the attributes from each raster to each WARP point by location. The result should be the creation of three additional 
  # columns (one for each raster) in the master WARP df, representing these values.

  # Should have: extracted values from BHS (the grizz_density estimate), Biophysical CS ( biophys raster) , 
  # and  griz_inc survey values (not cs, extracted from normal raster).

# Load Packages -----------------------------------------------------------
library(sf)
library(raster)
library(tidyverse)
library(dplyr)
library(sp)
library(terra)
library(rgdal)

# Bring in Data: ----------------------------------
  # Our WARP points master df:
warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.master.shp")
  # Our pres-abs ponts master df:
pres.abs.master <- ist_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /pres.abs.master.shp")
 
# Predictor Rasters:
biophys.cum.curmap <- rast("/Users/shannonspragg/ONA_grizz_Matt/data/processed/output/biophys_CS/cum_currmap.tif") # use this one
# Cumulative current flow shows the total current for each landscape pixel
# Normalized shows the degree to which a pixel has more or less current than expected under resistance-free conditions (cumulative current flow divided by flow potential)

# Just survey response layer (not CS):
grizz.inc.raster <- rast("/Users/shannonspragg/rasters/grizz_inc.tif") #  the proportion of people within a census that 
# responded “I would like to see grizzlies increase or increase substantially” in response to “how would you like to see grizzly 
# populations respond in the next several years?” 

# BHS layer:
grizz.dens <- rast("/Users/shannonspragg/ONA_GRIZZ/Grizz Density rasters/grizz_dens.tif") # Estimated grizzly density for the region
plot(grizz.dens)

  # SOI Boundary and Raster for template:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")

soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km buffer raster


# Check / Set CRS for Raster and Points -----------------------------------
 # Match the projection and CRS of the current map to the resistance maps:
 # Here we use the grizz.inc raster to be the template crs

    # Bear Density (BHS) Estimate:
crs(grizz.dens) <- crs(soi.rast) 
crs(grizz.inc.raster) == crs(soi.rast) # Nice, this worked --> now in BC Albers EPSG 3005
    # Biophys Map:
crs(biophys.cum.curmap) <- crs(soi.rast) 
crs(biophys.cum.curmap) == crs(soi.rast) # Nice, this worked --> now in BC Albers EPSG 3005
  # GrizzInc Map:
crs(grizz.inc.raster) <- crs(soi.rast) 
crs(grizzinc.raster) == crs(soi.rast) # Nice, this worked --> now in BC Albers EPSG 3005


  # Project SOI boundary:
soi.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(grizz.inc.raster))

st_crs(warp.all.sp) # This is in NAD83 BC Albers - EPSG 3005

  # Match the sf points CRS directly to the resistance raster:
warp.reproj <- st_make_valid(warp.all.sp) %>% 
  st_transform(crs=crs(soi.rast))
  
pres.abs.reproj <- st_make_valid(pres.abs.master) %>% 
  st_transform(crs=crs(soi.rast))
plot(st_geometry(pres.abs.reproj))
st_crs(pres.abs.reproj)
st_crs(warp.reproj)
crs(soi.rast) # The same as above, just formatted differently - success!

  # Check Raster Resolutions:
str(grizz.dens)
res(biophys.cum.curmap) # 1000 x 1000
res(grizz.inc.raster) # 1000 x 1000
res(soi.rast)


# Buffer the WARP Points (Before Overlay) --------------------------------------------------
  # Here we buffer the WARP and ppres-abs points by 5km before extracting the attributes from the current maps
warp.all.buf <- warp.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(warp.all.buf)) # Check the buffers

pres.abs.buf <- pres.abs.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers


  # Let's Turn the Buffered Points into a SpatVector:
warp.sv.buf <- vect(warp.all.buf)
pres.abs.sv.buf <- vect(pres.abs.buf)

  # Plot them together to see if projection truly is same:
plot(grizz.inc.raster)
plot(warp.sv.buf, add = TRUE) 

plot(grizz.dens)
plot(warp.sv.buf, add = TRUE) 

plot(soi.rast)
plot(warp.sv.buf, add = TRUE) 

plot(biophys.cum.curmap)
plot(warp.sv.buf, add = TRUE) 

plot(soi.rast)
plot(pres.abs.sv.buf, add= TRUE)

# Overlay WARP Points with CS Raster  --------------------------------------
  # Here we extract the mean values from each raster to the buffered points
warp.biophys.b.ext <- terra::extract(biophys.cum.curmap, warp.sv.buf, mean, na.rm = TRUE)  # This gives us the mean value of each buffered area --> what we want!
warp.grizz.inc.b.ext <- terra::extract(grizz.inc.raster, warp.sv.buf, mean, na.rm = TRUE) 
warp.bhs.b.extract <- terra::extract(grizz.dens, warp.sv.buf, mean, na.rm = TRUE) 

pres.abs.biophys.b.ext <- terra::extract(biophys.cum.curmap, pres.abs.sv.buf, mean, na.rm = TRUE)  # This gives us the mean value of each buffered area --> what we want!
pres.abs.grizz.inc.b.ext <- terra::extract(grizz.inc.raster, pres.abs.sv.buf, mean, na.rm = TRUE) 
pres.abs.bhs.b.extract <- terra::extract(grizz.dens, pres.abs.sv.buf, mean, na.rm = TRUE) 

  # Create New Column(s) for Extracted Values:
warp.reproj$BphysEx <- warp.biophys.b.ext[,2]   # MAke sure col names match!
warp.reproj$GrzzInE <- warp.grizz.inc.b.ext[,2]
warp.reproj$BHSExtr <- warp.bhs.b.extract[,2]

pres.abs.reproj$BphysEx <- pres.abs.biophys.b.ext[,2]
pres.abs.reproj$GrzzInE <- pres.abs.grizz.inc.b.ext[,2]
pres.abs.reproj$BHSExtr <- pres.abs.bhs.b.extract[,2]

# Check for NA's:
which(is.na(warp.reproj$BphysEx)) #none
which(is.na(warp.reproj$BHSExtr)) #none
which(is.na(warp.reproj$GrzzInE)) # none
which(is.na(pres.abs.reproj$BphysEx)) #none
which(is.na(pres.abs.reproj$BHSExtr)) #none
which(is.na(pres.abs.reproj$GrzzInE)) # none


# Save this as new file ---------------------------------------------------

st_write(warp.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.final.shp")
st_write(pres.abs.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /pres.abs.final.shp")



# Check for NA's: ---------------------------------------------------------
which(is.na(warp.reproj$BphysEx)) # We have about 70 NA's here..
#which(is.na(warp.reproj$SrvyRsE)) # NO NA's
which(is.na(warp.reproj$GrzzInE)) # 20 NA's
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

droprecords <- warp.reproj %>% drop_na(BphysEx) %>% drop_na(GrzzInE)
warp.dropped <- droprecords %>% drop_na(BHSExtr) %>% drop_na(GrzzInE)
warp.dropped <- droprecords %>% drop_na(GrzzInE)


# Check to see if the NA's we wanted removed are gone:
which(is.na(warp.dropped$BphysEx)) # Nice, this removed the NA's
which(is.na(warp.dropped$BHSExtr)) # This cleared the NA's
which(is.na(warp.dropped$SrvyRsE))
which(is.na(warp.dropped$GrzzInE)) # NO NA's
which(is.na(warp.dropped$Dm_Fr_T)) # NO NA's
which(is.na(warp.dropped$Ttl_F_C)) # NO NA's

# Nice, now we are 76 rows fewer - not bad

# Replace NA Values with Zero ---------------------------------------------
  # It seems like all the NA values on those two rasters result from a point being right on the edge, where
  # there isn't a current value to begin with. So if we replace them with zeros, I don't think this is a big deal.
warp.reproj.no.na <- mutate_at(warp.reproj, c("BphysEx", "BHSExtr"), ~replace(., is.na(.), 0))

which(is.na(warp.reproj.no.na$BiophysExtract)) # This made them all 0
which(is.na(warp.reproj.no.na$BHSExtract)) # Same as above

# Make sure all of our covariate columns have 0 NA's : CHECK!

which(is.na(warp.reproj.no.na$ds__PA_)) 
which(is.na(warp.reproj.no.na$dstn___)) 
which(is.na(warp.reproj.no.na$Dm_Fr_T))
which(is.na(warp.reproj.no.na$Ttl_F_C))
which(is.na(warp.reproj.no.na$BphysEx))
#which(is.na(warp.reproj.no.na$SurveyResistExtract))
which(is.na(warp.reproj.no.na$GrizzIncE))
which(is.na(warp.reproj.no.na$BHSExtr)) 
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
