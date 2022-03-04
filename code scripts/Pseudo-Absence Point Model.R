# Pseudo-absence Binary Model: --------------------------------------------
  #### Here we will generate pseudo-absences to represent our 0's for the binary model with bear conflict reports (1's)


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
install.packages("dismo")
library(dismo)
library(stars)

# Bring in Data: ----------------------------------------------------------
soi.10k.buf <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")
warp.ccs.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10_ccs.shp")

# Make SOI into Raster: ---------------------------------------------------
soi.rast <- st_rasterize(soi.10k.buf)

# export as tiff:
write_stars(soi.rast, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif")

#bring it in:
soi.rast <- raster("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif")

# Generate Random Points for Pseudo-absences: -----------------------------
set.seed(2345)
p.abs.pts <- randomPoints(soi.rast, 4000)

plot(soi.rast)
plot(p.abs.pts, add=TRUE) # This gives us our absence points!



