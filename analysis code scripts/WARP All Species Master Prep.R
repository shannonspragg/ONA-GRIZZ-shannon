# Creating a WARP All Species Master Data frames ----------------------------
  ## Here we add in the following predictor variables: distance to protected area, distance to metro area, dominant farm type,
  #  total farm count, and ccs region name/ID. The result of this script should be two semi-complete "master" data frames: one for
  #  the general conflict regression (with pres-abs points), and the second for the bear conflict regression (warp-only points)


# Loadport Packages -------------------------------------------------------
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
library(measurements)

# Bring in the Points Data -------------------------------
  #  WARP SOI 10km Buffer Data:
warp.all<-st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp_crop_10km_buf.shp") 
head(warp.all)
  # Bring in our pres abs data frame to get variables for our absences:
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp_pres.abs.shp")


# Bring in the Variable Data -----------------
  # Our SOI 10km Buffered Boundary:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km_buf.shp")
  # Filtered BC Protected Areas:
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/original/CAN Protected Areas/Parks_Combined2.shp") # Clayton's data

  # BC Metropolitan Areas:
bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/Data/original/BC Metro Areas/CNCNSSMTRR_polygon.shp")
str(bc.metro) # check this

  # Animal Product & Meat Farming:
animal.product.farming <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/Animal Product Farming.shp")
  # Ground Crop & Produce Production:
ground.crop.production <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/Ground Crop Production.shp")

  # BC CCS Regions:
bc.ccs<-st_read("//Users/shannonspragg/ONA_GRIZZ/Data/processed/BC CCS.shp")
str(bc.ccs)

  # SOI Raster for rasterizing later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km.tif") # SOI Region 10km buffer raster


# Reproject All Data ------------------------------------------------------
  # Now we project data to match the template raster:

bears.reproj <- st_make_valid(warp.all) %>%  # our warp data
  st_transform(crs=crs(soi.rast))
pres.abs.reproj <- st_make_valid(warp.pres.abs) %>%  # our pres-abs data
  st_transform(crs=crs(soi.rast))
bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=crs(soi.rast))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=crs(soi.rast))

farms.reproj <- st_make_valid(dominant.farms.bc) %>% 
  st_transform(crs=crs(soi.rast))
total.farms.reproj <- st_make_valid(total.farms.bc) %>% 
  st_transform(crs=crs(soi.rast))
soi.bound.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(soi.rast))

# Check to see if they match:
st_crs(bears.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(pres.abs.reproj) == st_crs(bears.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(total.farms.reproj) # [TRUE]

############################ Adding the Distance Variables to the Data:

# Prep Variable 1: Dist to PA's -------------------------------------------
  # Start by Filtering our PA's:
  ## NOTE: if using Clayton's data, skip filter part

  # Filter to those larger than 1,000 sq ha for general species (pres-abs):
#bc.PAs.1k.ha <- filter(bc.PAs.reproj, O_AREA > 1000) # Our 1k for general species
  # Filter to those larger than 10,000 sq ha for bears (warp):
#bc.PAs.10k.ha <- filter(bc.PAs.reproj, O_AREA > 10000) # Our 10k for bears only

#Calculation of the distance between the PA's and our points

  # Do this for our WARP only data:
dist.pts2pas.warp <- st_distance(bears.reproj, bc.PAs.reproj)
head(dist.pts2pas.warp)

  # And for our pres-abs data:
dist.pts2pas.presabs <- st_distance(pres.abs.reproj, bc.PAs.reproj)
head(dist.pts2pas.presabs)


  # Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.warp <- apply(dist.pts2pas.warp, 1, min)
str(min.dist.warp)

min.dist.presabs <- apply(dist.pts2pas.presabs, 1, min)
str(min.dist.presabs)

  # Add Distance Variable into Datatable:
bears.reproj$dist_to_PA<-min.dist.warp   
head(bears.reproj)

pres.abs.reproj$dist_to_PA<-min.dist.presabs
head(pres.abs.reproj)

  # Remove the units from the values (note: in meters)
as.numeric(bears.reproj$dist_to_PA)
as.numeric(pres.abs.reproj$dist_to_PA)

  # Convert units from meters to km:
bears.reproj$dist_to_PA<-conv_unit(bears.reproj$dist_to_PA,"m","km")
head(bears.reproj)

pres.abs.reproj$dist_to_PA<-conv_unit(pres.abs.reproj$dist_to_PA,"m","km")
head(pres.abs.reproj)

# Now each df has our Dist to PA column added in, and in km units

# Prep Variable 2: Dist to Metro Areas ------------------------------------
  #Calculation of the distance between the metro areas and our points

  # Do this for our WARP only data:
dist.pts2met.warp <- st_distance(bears.reproj, metro.reproj)
head(dist.pts2met.warp)

  # And the pres-abs data:
dist.pts2met.presabs <- st_distance(pres.abs.reproj, metro.reproj)
head(dist.pts2met.presabs)

  # Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.met.warp <- apply(dist.pts2met.warp, 1, min)
min.dist.met.presabs <- apply(dist.pts2met.presabs, 1, min)

  # Add Distance Variable into Data table
bears.reproj$dist_to_Metro<-min.dist.met.warp
head(bears.reproj)

pres.abs.reproj$dist_to_Metro<-min.dist.met.presabs
head(pres.abs.reproj)

  # Remove the units from the values (note: in meters)
as.numeric(bears.reproj$dist_to_Metro)  # Make sure col name matches data
as.numeric(pres.abs.reproj$dist_to_Metro)

  # Convert units from meters to km:
bears.reproj$dist_to_Metro<-conv_unit(bears.reproj$dist_to_Metro,"m","km")
head(bears.reproj)

pres.abs.reproj$dist_to_Metro<-conv_unit(pres.abs.reproj$dist_to_Metro,"m","km")
head(pres.abs.reproj)

  # This added the dist to metro areas column to our data


  # Check for NA's quick:
which(is.na(bears.reproj$dist_to_PA))
which(is.na(bears.reproj$dist_to_Metro))

which(is.na(pres.abs.reproj$dist_to_PA))
which(is.na(pres.abs.reproj$dist_to_Metro))


############################# Prepare Distance to Extent Bear Populations Variable: -------------------






############################## Adding the Agriculture Predictors to Our Data:

# Rasterize Farm Data & WARP Points ---------------------------------------
  ## Here we make rasters for the farm type categories within our SOI region:

  # Make these spat vectors:
animal.prod.sv <- vect(animal.product.farming)
ground.crop.sv <- vect(ground.crop.production)

  # Rasterize our subset rasters:
animal.prod.rast <- terra::rasterize(animal.prod.sv, soi.rast, field = "Farms_per_sq_km")
ground.crop.rast <- terra::rasterize(ground.crop.sv, soi.rast, field = "Farms_per_sq_km")

  # Fix the column names:
names(animal.prod.rast)[names(animal.prod.rast) == "Farms_per_sq_km"] <- "Density of Animal Product & Meat Farming / sq km"
names(ground.crop.rast)[names(ground.crop.rast) == "Farms_per_sq_km"] <- "Density of Ground Crop & Produce Farming / sq km"


  # Save these Farm Rasters:
terra::writeRaster(animal.prod.rast, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/animal_production_density_raster.tif")
terra::writeRaster(ground.crop.rast, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/ground_crop_density_raster.tif" )



# Buffer WARP Points Before Attributing Farm Values -----------------------
  # Here we buffer the WARP and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
bears.buf <- bears.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(bears.buf)) # Check the buffers

pres.abs.buf <- pres.abs.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

  # Make the buffered points spat vectors:
bears.sv.buf <- vect(bears.buf)
pres.abs.sv.buf <- vect(pres.abs.buf)

# Prep Variable 3: the Dominant Ag Type & Total Farm Count by CCS ----------------------------
  # Here I will extract the mean values from each raster to the buffered points

  # First, for our WARP data:
bears.farm.type.ext <- terra::extract(farm.type.rast, bears.sv.buf, modal, na.rm = TRUE) 
# This gives us the mean value of each buffered area --> what we want!
bears.total.farm.ext <- terra::extract(farm.count.rast, bears.sv.buf, mean, na.rm = TRUE) 

# Next, for our pres abs data:
pres.abs.farm.type.ext <- terra::extract(farm.type.rast, pres.abs.sv.buf, modal, na.rm = TRUE) 
# This gives us the mean value of each buffered area --> what we want!
pres.abs.total.farm.ext <- terra::extract(farm.count.rast, pres.abs.sv.buf, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
bears.reproj$Dm_Fr_T <- bears.farm.type.ext[,2]
bears.reproj$Ttl_F_C <- bears.total.farm.ext[,2]

pres.abs.reproj$Dm_Fr_T <- pres.abs.farm.type.ext[,2]
pres.abs.reproj$Ttl_F_C <- pres.abs.total.farm.ext[,2]

# Check for NA's quick:
which(is.na(bears.reproj$Dm_Fr_T))
which(is.na(bears.reproj$Ttl_F_C))

which(is.na(pres.abs.reproj$Dm_Fr_T))
which(is.na(pres.abs.reproj$Ttl_F_C))

############################ Next, Add in the CCS Region Names to the Data:


# Project the CCS Regions to match our data: ------------------------------
bc.ccs.reproj <- st_transform(bc.ccs, st_crs(soi.bound.reproj))

  # Check to see if the projections match:
st_crs(bc.ccs.reproj) == st_crs(soi.bound.reproj) # [TRUE] 

st_crs(bears.reproj) == st_crs(bc.ccs.crop) # [TRUE] 
st_crs(pres.abs.reproj) == st_crs(bc.ccs.crop) # [TRUE] 

  # Plot these together to make sure:
plot(st_geometry(bc.ccs.reproj))
plot(st_geometry(soi.bound.reproj), add=TRUE)

plot(st_geometry(soi.bound.reproj))
plot(st_geometry(bears.reproj), add=TRUE)

# Crop CCS Down to SOI 10km Extent: --------------------------------------------
soi.ccs.crop <- st_intersection(bc.ccs.reproj, soi.10k.buf)

  # Write this as a .shp for later:
st_write(soi.ccs.crop, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

# Assign the WARP Points to a CCS Region: ---------------------------------
  ## Here we want to overlay the points with the regions, adding a column in the warp data that is CCS region ID, 
  #  make sure this is a factor, to fit this as a varying intercept

  # Assign our points to a CCS category:
warp.ccs.join <- st_join(bears.reproj, left = TRUE, soi.ccs.crop) # join points

pres.abs.ccs.join <- st_join(pres.abs.reproj, left = TRUE, soi.ccs.crop) # join points

head(warp.ccs.join) # Assigned points to a CCS category
head(pres.abs.ccs.join) # nice

  # Delete the columns we don't want:
warp.ccs.join$PRNAME <- NULL
warp.ccs.join$PRNTCDVSNC <- NULL
warp.ccs.join$PRUID <- NULL
warp.ccs.join$CDUID <- NULL
warp.ccs.join$CPRVNCCD <- NULL
warp.ccs.join$FTRCD <- NULL
warp.ccs.join$CPRVNCCD <- NULL
warp.ccs.join$PRNTCDVSNC <- NULL
warp.ccs.join$FFCTVDT <- NULL
warp.ccs.join$XPRDT <- NULL
warp.ccs.join$OBJECTID <- NULL
warp.ccs.join$AREA_SQM <- NULL
warp.ccs.join$FEAT_LEN <- NULL 
warp.ccs.join$CDNAME <- NULL
warp.ccs.join$CDTYPE <- NULL
warp.ccs.join$CPRVNCNM <- NULL
warp.ccs.join$CCSNAME.x <- NULL
warp.ccs.join$CCSUID.x <- NULL

pres.abs.ccs.join$PRNAME <- NULL
pres.abs.ccs.join$PRNTCDVSNC <- NULL
pres.abs.ccs.join$PRUID <- NULL
pres.abs.ccs.join$CDUID <- NULL
pres.abs.ccs.join$CPRVNCCD <- NULL
pres.abs.ccs.join$FTRCD <- NULL
pres.abs.ccs.join$CPRVNCCD <- NULL
pres.abs.ccs.join$PRNTCDVSNC <- NULL
pres.abs.ccs.join$FFCTVDT <- NULL
pres.abs.ccs.join$XPRDT <- NULL
pres.abs.ccs.join$OBJECTID <- NULL
pres.abs.ccs.join$AREA_SQM <- NULL
pres.abs.ccs.join$FEAT_LEN <- NULL 
pres.abs.ccs.join$CDNAME <- NULL
pres.abs.ccs.join$CDTYPE <- NULL
pres.abs.ccs.join$CPRVNCNM <- NULL
pres.abs.ccs.join$CCSNAME.x <- NULL
pres.abs.ccs.join$CCSUID.x <- NULL

  # Rename these quick:
names(warp.ccs.join)[names(warp.ccs.join) == "CCSUID.y"] <- "CCSUID"
names(warp.ccs.join)[names(warp.ccs.join) == "CCSNAME.y"] <- "CCSNAME"
names(pres.abs.ccs.join)[names(pres.abs.ccs.join) == "CCSUID.y"] <- "CCSUID"
names(pres.abs.ccs.join)[names(pres.abs.ccs.join) == "CCSNAME.y"] <- "CCSNAME"

head(warp.ccs.join)
head(pres.abs.ccs.join)

# Check for NA's: -------------------------------------------------------

which(is.na(warp.ccs.join$CCSNAME)) # no NAs
which(is.na(warp.ccs.join$CCSUID)) # yay!!

which(is.na(pres.abs.ccs.join$CCSNAME)) # no NAs
which(is.na(pres.abs.ccs.join$CCSUID)) # yay!!

# WARP All Species Master Data Frame --------------------------------------
  # Save the resulting data frames here:
st_write(warp.ccs.join, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.master.shp")
st_write(pres.abs.ccs.join, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /pres.abs.master.shp")

