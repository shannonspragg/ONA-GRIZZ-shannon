
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
p.gen.conf.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/prob_general_conf.tif")

# Grizzinc:
grizzinc.rast <- terra::rast("/Users/shannonspragg/rasters/grizz_inc_BC.tif")

# Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Grizz Density rasters/grizz_dens.tif")

# Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/ONA_grizz_Matt/data/processed/output/biophys_CS/cum_currmap.tif") # use this one

# SOI Region for plotting:
soi.10k.buf <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

# Dist 2 PA for Bears:
dist2pa.rast.bear.10k <- rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/10k_hadist2pa_raster.tif" )

# Check Projections: ------------------------------------------------------
crs(p.gen.conf.rast) == crs(grizzinc.rast) #TRUE
crs(grizzinc.rast) == crs(bhs.rast) #TRUE
crs(biophys.rast) == crs(bhs.rast) #TRUE
crs(dist2pa.rast.bear.10k) == crs(bhs.rast)

soi.reproj <- st_make_valid(soi.10k.buf) %>% 
  st_transform(crs=crs(soi.rast))

# Check Extents:
ext(soi.rast) # Need to match others to this one
ext(grizzinc.rast)
ext(biophys.rast)
ext(bhs.rast)

# Crop these Rasters:
grizzinc.crop <- terra::crop(grizzinc.rast, p.gen.conf.rast)
biophys.crop <- terra::crop(biophys.rast, p.gen.conf.rast)
bhs.crop <- terra::crop(bhs.rast, p.gen.conf.rast)
d2pa.b.crop <- terra::crop(dist2pa.rast.bear.10k, p.gen.conf.rast)

# Resample to match extents and res:
grizzinc.rsmple <- resample(grizzinc.crop, p.gen.conf.rast, method='bilinear')
biophys.rsmple <- resample(biophys.crop, p.gen.conf.rast, method='bilinear')
bhs.rsmple <- resample(bhs.crop, p.gen.conf.rast, method='bilinear')
d2pa.b.rsmpl <- resample(d2pa.b.crop, p.gen.conf.rast, method='bilinear')

# Plot Check:
soi.bound.vect <- vect(soi.reproj)

plot(grizzinc.rsmple)
plot(soi.bound.vect, add=TRUE)

plot(biophys.rsmple)
plot(soi.bound.vect, add=TRUE)

plot(bhs.rsmple)
plot(soi.bound.vect, add=TRUE)

plot(d2pa.b.rsmpl)
plot(soi.bound.vect, add=TRUE)

# Cut these down to the SOI Boundary: -------------------------------------

grizzinc.soi <- terra::mask(grizzinc.rsmple, soi.bound.vect) # BEA-UTIFUL!
biophys.soi <- terra::mask(biophys.rsmple, soi.bound.vect) # BEA-UTIFUL!
bhs.soi <- terra::mask(bhs.rsmple, soi.bound.vect) # BEA-UTIFUL!
d2pa.b.soi <- terra::mask(d2pa.b.rsmpl, soi.bound.vect) # BEA-UTIFUL!


plot(biophys.soi)
plot(bhs.soi)
plot(d2pa.b.soi)

# Fix the column names:
names(grizzinc.soi)[names(grizzinc.soi) == "grizz_inc"] <- "Support for Grizzly Increase"
names(biophys.soi)[names(biophys.soi) == "cum_curmap"] <- "Biophysical Connectivity Currentmap"
names(bhs.soi)[names(bhs.soi) == "Height"] <- "Bear Habitat Suitability (BHS)"



# Save our Cropped Rasters: -----------------------------------------------
terra::writeRaster(grizzinc.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/grizz_inc_SOI_10km.tif")
terra::writeRaster(biophys.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/biophys_SOI_10km.tif")
terra::writeRaster(bhs.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/bhs_SOI_10km.tif")
terra::writeRaster(d2pa.b.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/d2pa_bears_SOI_10km.tif")


