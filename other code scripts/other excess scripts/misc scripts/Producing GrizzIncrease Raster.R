# Prepping Survey (GrizzIncrease)Raster (from Combined Resist & Sociobio) ---------------
# Here I will use raster math to subtract the sociobio raster from the combined resistance,
# to produce the difference which represents the purely social layer, the GrizzIncrease raster.
## UPDATE: should be subtracting the biophys from sociobio


# Load Packages -----------------------------------------------------------
library(raster)
library(terra)

# Import the Raster Data --------------------------------------------------
# Here we import the already produced sociobio and combined resist rasters (from the ONA GRIZZ folder):
sociobio <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/sociobio_resist.tif")
comb.resist <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/bio_combined_resist.tif")

# Bring in the biophys only raster:
biophys <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/biophys_normalized_cum_currmap.tif")
plot(sociobio)
plot(comb.resist)
plot(biophys)

# Raster Math: Subtract to get Social Raster ------------------------------
# Here I subtract the sociobio raster from the combined resistance raster to get social (grizzIncrease)
social.2 <- sociobio - comb.resist # this gives us the difference - which is the + grizz inc
social.2
plot(social.2) # I think this shows us the difference - NICE!

# REVISED: do this by subtracting the biophys from sociobio (which is biophys + 1 support for grizzinc)
grizzinc <- sociobio - biophys
plot(grizzinc)

# Save the New Raster -----------------------------------------------------
# Let's write this as a tif file and save it for use in omniscape

writeRaster(social.2, "/Users/shannonspragg/rasters/GrizzIncrease (Social)_2.tif")

writeRaster(grizzinc, "/Users/shannonspragg/rasters/GrizzIncrease Raster.tif")

# Let's confirm this saved properly:
social.test <- rast("/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/GrizzIncrease (Social)_2.tif")
plot(social.test)
social.test # Checked, this saved properly!

grizzinc.test <- rast("/Users/shannonspragg/rasters/GrizzIncrease Raster.tif")
plot(grizzinc.test)
grizzinc.test # Checked, this saved properly!

# Bring in Grizz Density Raster -------------------------------------------
# Bringing this in to check the dimensions and projection:
grizz.density <- rast("/Users/shannonspragg/ONA_GRIZZ/Grizz Density rasters/v2.gpkg")
grizz.density
plot(grizz.density)
plot(social.test)

# Match Extent:
ext(grizz.density) <- c(3700, 1946700, 300900, 1750900) #Match in extents

# Match CRS:
crs(grizz.density) <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Match Resolution:
# Want to aggregate to go from 500 to 1000 res:
grizz.agg <- raster::aggregate(grizz.density, fact=2)
grizz.agg
plot(grizz.agg)
res(grizz.density) <- c(1000,1000) # Now they match in resolution

grizz.dens.resampl <- resample(grizz.density, social.test)
plot(grizz.dens.resampl) 

# Write this as the updated raster:
writeRaster(grizz.dens.resampl, "/Users/shannonspragg/rasters/grizz_dens.tif")


# Try this with Hucks Raster  -----------------------------
huck.occurance <- rast("/Users/shannonspragg/rasters/VACCMEM_Occ.tif")
huck.kcal <- rast("/Users/shannonspragg/rasters/VACCMEM_kcal.tif")
huck.occurance
plot(huck.occurance)
huck.kcal
plot(huck.kcal)

# Match Extent:
ext(huck.occurance) <- c(3700, 1946700, 300900, 1750900) #Match in extents
ext(huck.kcal) <- c(3700, 1946700, 300900, 1750900) #Match in extents

# Match CRS:
crs(huck.occurance) <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"
crs(huck.kcal) <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Match Resolution:
huck.resampl <- resample(huck.occurance, comb.resist)
huck.resampl
huck.kcal.resampl <- resample(huck.kcal, comb.resist)

st_crs(huck.occurance)
plot(huck.kcal.resampl)

# Write this as the updated raster:
writeRaster(huck.resampl, "/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/Huck_Occur_adjusted.tif")
writeRaster(huck.kcal.resampl, "/Users/shannonspragg/ONA_GRIZZ/Sociobio, Resist, & Survey Rasters/Huck_kcal_adjusted.tif")


