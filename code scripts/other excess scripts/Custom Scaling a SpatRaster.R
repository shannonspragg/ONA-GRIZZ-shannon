# Scaling a SpatRaster: ---------------------------------------------------
  # Here we have both a step-by-step customized scaling and write a function for scaling rasters in terra. We scale them
  # by subtracting the mean and diving by 2 standard deviations.


# Load Packages: ----------------------------------------------------------
library(terra)
library(dplyr)
library(tidyverse)


# Bring in Data: ----------------------------------------------------------
# Distance to Neartest Protected Area (km):
dist2pa.rast.general.1k <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/1k_ha_dist2pa_raster.tif" )

# Distance to Nearest Metro Area (km):
dist2met.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dist2metro_raster.tif" )




# Writing a Scale Function: -----------------------------------------------
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd.raster <-function(variable){(variable - global(variable, "mean", na.rm=TRUE)[,1])/(2*global(variable, "sd", na.rm=TRUE)[,1])}

dist2metro.rast.sc <- scale2sd.raster(dist2met.rast) # This is having issues by creating a dataframe
dist2pa.rast.sc <- scale2sd.raster(dist2pa.rast.general.1k)


# Step-by-Step Scaling: ---------------------------------------------------
# We can also do this Step by Step:

# Distance to PA:
d2pa.mean <- global(dist2pa.rast.general.1k, "mean")   # Obtain the mean values of the raster
d2pa.sub.mean <- dist2pa.rast.general.1k - d2pa.mean[,1]  # Subtract raster values by their means
d2pa.sd <- global(d2pa.sub.mean, "sd")  # Obtain the standard deviation of the raster values
d2pa.rast.sc <- d2pa.sub.mean / ( 2 * d2pa.sd[,1])  # Divide by 2 standard deviations

# Distance to Metro
d2met.mean <- global(dist2met.rast, "mean")
d2met.sub.mean <- dist2met.rast - d2met.mean[,1]
d2met.sd <- global(d2met.sub.mean, "sd")
d2met.rast.sc <- d2met.sub.mean / ( 2 * d2met.sd[,1])

# Check to see if these scaled:
d2pa.rast.sc
d2met.rast.sc
