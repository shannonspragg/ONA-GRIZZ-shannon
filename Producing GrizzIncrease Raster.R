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
# NOTE: Was not able to get this to work like I did with hucks... maybe use those
# Bringing this in to check the dimensions and projection:
grizz.density <- rast("/Users/shannonspragg/rasters/Clayton_griz_dens.tif")
grizz.density
plot(grizz.density)
plot(comb.resist)

# Match Extent:
ext(grizz.density) <- c(3700, 1946700, 300900, 1750900) #Match in extents

# Match CRS:
crs(grizz.density) <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Match Resolution:
# Want to aggregate to go from 500 to 1000 res:
grizz.agg <- raster::aggregate(grizz.density, fact=2)
grizz.agg

res(grizz.density) <- c(1000,1000) # Now they match in resolution

# grizz.resample <- resample(grizz.density, comb.resist, method="ngb") # Moves r values over to s - bilinear does a weighted average based on proximity
# plot(grizz.resample) # this won't work

grizz.extend <- terra::extend(grizz.agg, comb.resist)
grizz.extend
plot(grizz.extend)

# Write this as the updated raster:
writeRaster(grizz.density, "/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/Grizz_Dens_new_ext.tif")


# Try this with Hucks Raster - any different? YES! -----------------------------
huck.occurance <- rast("/Users/shannonspragg/rasters/VACCMEM_Occ.tif")
huck.occurance
plot(huck.occurance)

# Match Extent:
ext(huck.occurance) <- c(3700, 1946700, 300900, 1750900) #Match in extents

# Match CRS:
crs(huck.occurance) <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Match Resolution:
huck.resampl <- resample(huck.agg, comb.resist)
huck.resampl
plot(huck.resampl)

# Write this as the updated raster:
writeRaster(huck.resampl, "/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/Huck_Occur_adjusted.tif")
