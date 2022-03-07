# Prepping Human Density for SOI Region: ----------------------------------
    ## Prepping a predictor for human density (1km NASA human pop dataset) → controls for reports occurring 
    # where people likely aren’t (crop this down to our SOI area)


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
#install.packages("dismo")
library(dismo)
library(stars)


# Import Data: ------------------------------------------------------------
world.hum.dens <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/Human Pop Density/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2020_1_deg.tif")

soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km

soi.10k.buf <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

# Reproject the Rasters: --------------------------------------------------
world.dens.reproj <- terra::project(world.hum.dens, crs(soi.rast))

crs(world.dens.reproj) == crs(soi.rast) #TRUE

soi.reproj <- st_make_valid(soi.10k.buf) %>% 
  st_transform(crs=crs(soi.rast))


# Crop the Human Density Data to SOI: -------------------------------------

hum.dens.crop <- terra::crop(world.dens.reproj, soi.rast)

hm.dens.rsmple <- resample(hum.dens.crop, soi.rast, method='bilinear')


# Overlay the SOI Boundary: -----------------------------------------------
soi.bound.vect <- vect(soi.reproj)


plot(hm.dens.rsmple)
plot(soi.bound.vect, add=TRUE) # We see they're projected properly and have a nice overlay going

hm.dens.soi <- terra::mask(hm.dens.rsmple, soi.bound.vect) # BEA-UTIFUL!

str(hm.dens.soi) # Res is 1463 x 1463


# Save Raster as .tif: ----------------------------------------------------

terra::writeRaster(hm.dens.soi, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/Human Pop Density/human_dens_SOI_10km.tif")



# Add Human Dens as Predictor: --------------------------------------------
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_pres_abs_10km_ccs.shp")


# Project Data: -----------------------------------------------------------
warp.pa.reproj <- st_make_valid(warp.pres.abs) %>% 
  st_transform(crs=crs(hm.dens.soi))


# Buffer the WARP Points (Before Overlay) --------------------------------------------------
# Here we buffer the WARP points by 5km before extracting the attributes from the current maps
warp.ps.buf <- warp.pa.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(warp.ps.buf)) # Check the buffers

st_crs(warp.ps.buf)

# Need to make points a SpatVector:
warp.ps.sv <- vect(warp.ps.buf)
str(warp.ps.sv)
crs(warp.ps.sv)

# Plot them together to see if projection is same:
plot(hm.dens.soi)
plot(warp.ps.sv, add = TRUE) # HECK YES

# Extract Values to Points: -----------------------------------------------

# Here I will extract the mean values from each raster to the buffered points

warp.dens.b.ext <- terra::extract(hm.dens.soi, warp.ps.sv, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
warp.pa.reproj$Human_Dens <- warp.dens.b.ext[,2] # WHat units are these in?? Number of persons per square kilometer


# Check for NA's:
which(is.na(warp.pa.reproj$Human_Dens)) #We have like 6 of these

# Plot these:
plot(hm.dens.soi)
plot(st_geometry(warp.pa.reproj[4186,]), col = "red", add = TRUE)
# This includes the point value --> no NA

# Replace NA Values with Zero ---------------------------------------------
# It seems like all the NA values on those two rasters result from a point being right on the edge, where
# there isn't a current value to begin with. So if we replace them with zeros, I don't think this is a big deal.
warp.reproj.no.na <- mutate_at(warp.pa.reproj, c("Human_Dens"), ~replace(., is.na(.), 0))

which(is.na(warp.reproj.no.na$Human_Dens)) # This made them all 0


# Save New Pres-abs DF with Human Dens: -----------------------------------
st_write(warp.reproj.no.na, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_hum_dens.shp")

