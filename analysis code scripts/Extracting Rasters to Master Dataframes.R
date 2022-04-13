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
warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp.master.shp")
  # Our pres-abs ponts master df:
pres.abs.master <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/pres.abs.master.shp")

  # Predictor Rasters:
biophys.cum.curmap <- rast("/Users/shannonspragg/ONA_grizz_Matt/data/processed/output/biophys_CS/cum_currmap.tif") # use this one
  # Cumulative current flow shows the total current for each landscape pixel
  # Normalized shows the degree to which a pixel has more or less current than expected under resistance-free conditions (cumulative current flow divided by flow potential)

  # Just survey response layer (not CS):
grizz.inc.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/original/Grizz Increase/grizz.increase.map.fixed.tif") #  the proportion of people within a census that 

  # responded “I would like to see grizzlies increase or increase substantially” in response to “how would you like to see grizzly 
  # populations respond in the next several years?” 

  # BHS layer:
grizz.dens <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/original/Grizz Density/grizz_dens.tif") # Estimated grizzly density for the region
plot(grizz.dens)

  # SOI Boundary and Raster for template:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km_buf.shp")
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km.tif") # SOI Region 10km buffer raster

 # Human Density for SOI:
hm.dens.soi <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/human_dens_SOI_10km.tif") # SOI Region 10km


# Check / Set CRS for Raster and Points -----------------------------------
  # Match the projection and CRS of the current map to the resistance maps:
  # Here we use the grizz.inc raster to be the template crs

# GrizzInc Map:
grizz.inc.reproj <- terra::project(grizz.inc.rast, crs(soi.rast))
crs(grizz.inc.reproj) == crs(soi.rast) 
  # Bear Density (BHS) Estimate:
grizz.dens.reproj <- terra::project(grizz.dens, crs(soi.rast))
crs(grizz.dens) == crs(soi.rast) # Nice, this worked --> now in BC Albers EPSG 3005
  # Biophys Map:
biophys.reproj <- terra::project(biophys.cum.curmap, crs(soi.rast))
crs(biophys.cum.curmap) == crs(soi.rast) # Nice, this worked --> now in BC Albers EPSG 3005
# Nice, this worked --> now in BC Albers EPSG 3005
  # Human Density:
hm.dens.reproj <- terra::project(hm.dens.soi, crs(soi.rast))
crs(hm.dens.soi) == crs(soi.rast) # Nice, this worked --> now in BC Albers EPSG 3005

  # Project SOI boundary:
soi.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(soi.rast))

st_crs(warp.all.sp) # This is in NAD83 BC Albers - EPSG 3005

  # Match the sf points CRS directly to the template raster:
warp.reproj <- st_make_valid(warp.all.sp) %>% 
  st_transform(crs=crs(soi.rast))

pres.abs.reproj <- st_make_valid(pres.abs.master) %>% 
  st_transform(crs=crs(soi.rast))
st_crs(pres.abs.reproj)
st_crs(warp.reproj)
crs(soi.rast) # The same as above, just formatted differently - success!

  # Check Raster Resolutions:
res(grizz.dens)
res(biophys.cum.curmap) # 1000 x 1000
res(grizz.inc.raster) # 270 x 270
res(soi.rast)
res(hm.dens.soi)

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
soi.sv <- vect(soi.bound.reproj)

# Plot them together to see if projection truly is same:
plot(grizz.inc.reproj)
plot(warp.sv.buf, add = TRUE) 

plot(grizz.dens.reproj)
plot(warp.sv.buf, add = TRUE) 

plot(biophys.reproj)
plot(warp.sv.buf, add = TRUE) 

plot(hm.dens.reproj)
plot(pres.abs.sv.buf, add = TRUE) 


# Crop these Rasters:
grizzinc.crop <- terra::crop(grizz.inc.reproj, soi.rast)  #MAKE SURE THIS IS UPDATED
biophys.crop <- terra::crop(biophys.reproj, soi.rast)
bhs.crop <- terra::crop(grizz.dens.reproj, soi.rast)

# Resample to match extents and res ( we want to match to the grizzinc res):
soi.rsmple <- resample( soi.rast,grizzinc.crop, method='bilinear')
biophys.rsmple <- resample(biophys.crop, soi.rsmple, method='bilinear')
bhs.rsmple <- resample(bhs.crop, soi.rsmple, method='bilinear')
hm.dens.rsmple <- resample(hm.dens.reproj, soi.rsmple, method='bilinear')

grizzinc.soi <- terra::mask(grizzinc.rsmple, soi.bound.vect) 
biophys.soi <- terra::mask(biophys.rsmple, soi.bound.vect) 
bhs.soi <- terra::mask(bhs.rsmple, soi.bound.vect) 

# Overlay WARP Points with CS Raster  --------------------------------------
# Here we extract the mean values from each raster to the buffered points
warp.biophys.b.ext <- terra::extract(biophys.rsmple, warp.sv.buf, mean, na.rm = TRUE)  # This gives us the mean value of each buffered area --> what we want!
warp.grizz.inc.b.ext <- terra::extract(grizzinc.crop, warp.sv.buf, mean, na.rm = TRUE) 
warp.bhs.b.extract <- terra::extract(bhs.rsmple, warp.sv.buf, mean, na.rm = TRUE) 
warp.dens.b.ext <- terra::extract(hm.dens.rsmple, warp.sv.buf, mean, na.rm = TRUE) 

pres.abs.biophys.b.ext <- terra::extract(biophys.rsmple, pres.abs.sv.buf, mean, na.rm = TRUE)  # This gives us the mean value of each buffered area --> what we want!
pres.abs.grizz.inc.b.ext <- terra::extract(grizzinc.crop, pres.abs.sv.buf, mean, na.rm = TRUE) 
pres.abs.bhs.b.extract <- terra::extract(bhs.rsmple, pres.abs.sv.buf, mean, na.rm = TRUE) 
pres.abs.dens.b.ext <- terra::extract(hm.dens.rsmple, pres.abs.sv.buf, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
warp.reproj$Biophys <- warp.biophys.b.ext[,2]   # MAke sure col names match!
warp.reproj$GrizzInc <- warp.grizz.inc.b.ext[,2]
warp.reproj$BHS <- warp.bhs.b.extract[,2]
warp.reproj$Human_Dens <- warp.dens.b.ext[,2] # Number of persons per square kilometer

pres.abs.reproj$Biophys <- pres.abs.biophys.b.ext[,2]
pres.abs.reproj$GrizzInc <- pres.abs.grizz.inc.b.ext[,2]
pres.abs.reproj$BHS <- pres.abs.bhs.b.extract[,2]
pres.abs.reproj$Human_Dens <- pres.abs.dens.b.ext[,2] 

# Check for NA's:
which(is.na(warp.reproj$BphysEx)) #none
which(is.na(warp.reproj$BHSExtr)) #none
which(is.na(warp.reproj$GrzzInE)) # none
which(is.na(warp.reproj$Human_Dens)) 

which(is.na(pres.abs.reproj$BphysEx)) #none
which(is.na(pres.abs.reproj$BHSExtr)) #none
which(is.na(pres.abs.reproj$GrzzInE)) # none
which(is.na(pres.abs.reproj$Human_Dens)) 


# Save this as new file ---------------------------------------------------

st_write(warp.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.final.shp")
st_write(pres.abs.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /pres.abs.final.shp")

