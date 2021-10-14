# Prepping Survey (GrizzIncrease)Raster (from Combined Resist & Sociobio) ---------------
# Here I will use raster math to subtract the sociobio raster from the combined resistance,
# to produce the difference which represents the purely social layer, the GrizzIncrease raster.


# Load Packages -----------------------------------------------------------
library(raster)
library(terra)

# Import the Raster Data --------------------------------------------------
# Here we import the already produced sociobio and combined resist rasters (from the ONA GRIZZ folder):
sociobio <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/sociobio_resist.tif")
comb.resist <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/combined_resist.tif")


