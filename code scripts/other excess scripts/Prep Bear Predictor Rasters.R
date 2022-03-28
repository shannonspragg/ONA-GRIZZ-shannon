
# Prep Bear Predictor Rasters: --------------------------------------------


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)
library(stars)


# Load Data: --------------------------------------------------------------
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km

# Grizzinc:
grizzinc.rast <- terra::rast("/Users/shannonspragg/rasters/grizz_inc_BC.tif")

# Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Grizz Density rasters/grizz_dens.tif")

# Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/ONA_grizz_Matt/data/processed/output/biophys_CS/cum_currmap.tif") # use this one

# SOI Region for plotting:
soi.10k.buf <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

# Check Projections: ------------------------------------------------------
crs(soi.rast) == crs(grizzinc.rast) #TRUE
crs(grizzinc.rast) == crs(bhs.rast) #TRUE
crs(biophys.rast) == crs(bhs.rast) #TRUE

soi.reproj <- st_make_valid(soi.10k.buf) %>% 
  st_transform(crs=crs(soi.rast))

# Check Extents:
ext(soi.rast) # Need to match others to this one
ext(grizzinc.rast)
ext(biophys.rast)
ext(bhs.rast)

# Crop these Rasters:
grizzinc.crop <- terra::crop(grizzinc.rast, soi.rast)
biophys.crop <- terra::crop(biophys.rast, soi.rast)
bhs.crop <- terra::crop(bhs.rast, soi.rast)

# Plot Check:
soi.bound.vect <- vect(soi.reproj)

plot(grizzinc.crop)
plot(soi.bound.vect, add=TRUE)

plot(biophys.crop)
plot(soi.bound.vect, add=TRUE)

plot(bhs.crop)
plot(soi.bound.vect, add=TRUE)


# Save our Cropped Rasters: -----------------------------------------------
terra::writeRaster(grizzinc.crop, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/grizz_inc_SOI_10km.tif")
terra::writeRaster(biophys.crop, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/biophys_SOI_10km.tif")
terra::writeRaster(bhs.crop, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/bhs_SOI_10km.tif")


