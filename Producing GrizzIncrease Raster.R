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
plot(sociobio)
plot(comb.resist)


# Raster Math: Subtract to get Social Raster ------------------------------
# Here I subtract the sociobio raster from the combined resistance raster to get social (grizzIncrease)
social <- comb.resist - sociobio
social
plot(social) # Yep, this shows us the difference - NICE!


# Save the New Raster -----------------------------------------------------
# Let's write this as a tif file and save it for use in omniscape

writeRaster(social, "/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/GrizzIncrease (Social).tif")

# Let's confirm this saved properly:
social.test <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/GrizzIncrease (Social).tif")
plot(social.test)
social.test # Checked, this saved properly!


# Bring in Grizz Density Raster -------------------------------------------
# Bringing this in to check the dimensions and projection:
grizz.density <- rast("/Users/shannonspragg/rasters/Clayton_griz_dens.tif")
grizz.density
plot(grizz.density)
comb.resist
ext(grizz.density) <- c(3700, 1946700, 300900, 1750900)

# Attempt to match projections:
grizz.dens.reproj <- terra::project(grizz.density, comb.resist)