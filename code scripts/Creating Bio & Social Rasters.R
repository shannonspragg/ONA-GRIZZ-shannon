# Producing the Layers for Biophys and Social (Grizz Increase) ------------
  # Here we collect the datasets for the biophysical layer and create the socail grizz layer, ultimately producing
  # a sociobio file for omniscape analysis, a biophys layer for both omniscape analysis and regular resistace extraction,
  # and a social raster representing just the survey response resultant resistance to grizzlys
  # (This code is adapted from Matt's on ONA_Grizz)

##### NOTE: If we want to estimate the effects of opinions on conflict, we need to extract them directly from the resistance raster 
          # If we want to know how opinions might mix with biology to affect movement, we would want to extract from omniscape results 
          # based on the sociobio resistance surface


# Load Packges ------------------------------------------------------------
library(terra)
library(sf)
install.packages("here")
library(here)
library(rgdal)


# Load files --------------------------------------------------------------
ona_bdry <- st_read(here("/Users/shannonspragg/ONA_grizz_Matt/data/original/ona/ONA_TerritoryBound.shp"))
griz_dens <- rast(here("/Users/shannonspragg/ONA_grizz_Matt/data/original/griz_dens/v2.gpkg"))
hii <- rast("/Users/shannonspragg/ONA_grizz_Matt/data/original/hii_n_amer/")
elev.can <- rast(raster::getData('alt', country = 'CAN'))
elev.us <- rast(raster::getData('alt', country = 'USA')[[1]])
elev <- mosaic(elev.can, elev.us) # using mosaic to stitch these together

griz.resist <- rast("/Users/shannonspragg/ONA_grizz_Matt/data/original/social grizz/griz_inc.tif")


# Reproject the data: -----------------------------------------------------
ona_proj.sp <- ona_bdry %>% st_transform(., crs(griz_dens)) %>% st_buffer(., dist=5000) %>% as(., "Spatial")
ona_proj.vec <- vect(ona_proj.sp)
#griz_crop <- crop(griz_dens, ona_proj.sp)


griz.ext <- terra::extend(griz_dens, ona_proj.vec, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/griz_ext.tif"), overwrite=TRUE)
#griz.crop <- crop(griz.ext, ona_proj.vec)
griz.ext[is.nan(griz.ext)] <- 0

hii.proj <- terra::project(hii, griz.ext, method="bilinear")
hii.crop <- crop(hii.proj, griz.ext)

elev.proj <- terra::project(elev, griz.ext, method="bilinear")
elev.crop <- crop(elev.proj, griz.ext)
rough <- terrain(elev.crop, v="TRI")
rough.max <-  global(rough, "max", na.rm=TRUE)[1,]
rough.min <-  global(rough, "min", na.rm=TRUE)[1,]
rough.rescale <- (rough - rough.min)/(rough.max - rough.min)
rough.rescale[rough.rescale==0] <- 0.000000001
rough.rescale[is.nan(rough.rescale)] <- 1

hii.min <- global(hii.crop, "min", na.rm=TRUE)[1,]
hii.max <- global(hii.crop, "max", na.rm=TRUE)[1,]
hii.rescale <- (hii.crop - hii.min) / (hii.max - hii.min)
hii.rescale[hii.rescale == 0] <- 0.000000001
hii.rescale[is.nan(hii.rescale)] <- 1

griz.resist.proj <- terra::project(griz.resist, griz.ext, method="bilinear")
griz.resist.crop <- elev.crop <- crop(griz.resist.proj, griz.ext)
griz.resist.1m <- 1-griz.resist.crop
griz.resist.1m[is.nan(griz.resist.1m)] <- 1

griz.ext.min <-global(griz.ext, "min", na.rm=TRUE)[1,]
griz.ext.max <- global(griz.ext, "max", na.rm=TRUE)[1,] 
griz.ext.invert <- ((griz.ext - griz.ext.max)*-1) + griz.ext.min
griz.ext.invert[griz.ext.invert == 0] <- 0.000000001

griz.ext.nozero <- griz.ext
griz.ext.nozero[griz.ext.nozero==0] <- 0.0000000001
griz.ext.inv <- (griz.ext.nozero)^-1


# Create Combined Bio and Sociobio Layers: --------------------------------
biophys.combined <- hii.rescale + griz.ext.invert + rough.rescale # this gives us our full biophysical layer
social.biophys <- hii.rescale + griz.ext.invert + rough.rescale + griz.resist.1m # this gives us our biophysical + grizz resistance


# Write these as files: ---------------------------------------------------
writeRaster(hii.rescale, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/hii_resist.tif"), overwrite=TRUE)
writeRaster(griz.ext, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/griz_source.tif"), overwrite=TRUE)
writeRaster(griz.ext.invert, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/griz_resist.tif"), overwrite=TRUE)
writeRaster(griz.ext.inv, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/griz_resist_recip.tif"), overwrite=TRUE)
writeRaster(biophys.combined, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/biophys_resist.tif"), overwrite=TRUE)
writeRaster(social.biophys, filename=here("/Users/shannonspragg/ONA_grizz_Matt/data/processed/sociobio_resist.tif"), overwrite=TRUE)
