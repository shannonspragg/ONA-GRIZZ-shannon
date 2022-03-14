# Creating our CCS Raster: Varying Intercept for CCS Region: --------------
    ## Here we need to produce a raster where each CCS has the median posterior estimate of the 
#   mean of the random effect for that CCS. We can do so by making our CCS regions into a raster, saving the posterior mean estimates 
#   of the model with the random effect to our points df, making the points$posteriormean into a raster, and then extracting those
#   point values to each CCS region, then saving that as a raster.



# Load Packages: ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(fasterize)
library(terra)
library(stars)


# Bring in Data: ----------------------------------------------------------

soi.ccs <- st_read( "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

warp.pres.abs.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_post_preds.shp")

# Bring in one of our rasters for rasterizing polygon data later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km


# Reproject All Data ------------------------------------------------------
# Now we have the protected areas projected to match the biophys raster:
warp.pa.reproj <- st_make_valid(warp.pres.abs.df) %>% 
  st_transform(crs=crs(soi.rast))
soi.ccs.reproj <- st_make_valid(soi.ccs) %>% 
  st_transform(crs=crs(soi.rast))

# Check to see if they match:
st_crs(warp.pa.reproj) == st_crs(soi.ccs.reproj) # [TRUE] = These ARE now the same


# Make our CCS Raster: ----------------------------------------------------

# Make these spatvectors:
soi.ccs.sv <- vect(soi.ccs.reproj)
plot(soi.ccs.sv)

warp.ps.sv <- vect(warp.pa.reproj)

# Make our Raster:
soi.ccs.rast <- terra::rasterize(soi.ccs.sv, soi.rast, field = "CCSNAME")


# Make Raster for our Posterior Means for CCS Varying Intercept: ---------------------
post.means.rast <- terra::rasterize(warp.ps.sv, soi.rast, field = "Pst_M_P")

# Fix the column name:
names(post.means.rast)[names(post.means.rast) == "SOI_10km"] <- "Posterior Mean Estimate"

# Extract Values of Posterior Means to CCS regions: --------------------------------------

warp.post.mean.ext <- terra::extract(post.means.rast, soi.ccs.sv, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
soi.ccs.sv$PostMean <- warp.post.mean.ext[,2] 


# Make our CCS Post Mean Raster: ------------------------------------------

ccs.post.means.rast <- terra::rasterize(soi.ccs.sv, soi.rast, field = "PostMean")

# Check this:

plot(warp.ps.sv)
plot(ccs.post.means.rast, add=TRUE) #HECK YEAHHHH


# Save our CCS Post Means Raster: -----------------------------------------

terra::writeRaster(ccs.post.means.rast, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/CCS_postmeans_raster.tif" )








