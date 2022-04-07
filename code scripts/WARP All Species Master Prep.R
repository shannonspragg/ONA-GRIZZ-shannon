# Creating a WARP All Species Master Data frames ----------------------------
# This is going to be a longer script, but hopefully we can condense this efficiently 
# given the cleaned up scripts this will be compiled from.
# The following code will be re-running the variables (dist to PA's, dist to Metro, 
# Farm Type, and Farm count) for the all-species full year data and then compiling that 
# into a full-species master df for the binomial regression


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
warp.all<-read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10km_buf.shp") 
head(warp.all)
  # Bring in our pres abs data frame to get variables for our absences:
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.need.farms.shp")


# Bring in the Variable Data -----------------
  # Our SOI 10km Buffered Boundary:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")
  # Filtered BC Protected Areas:
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")
  # BC Metropolitan Areas:
bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census metro areas/CEN_CENSUS_METRO_AREAS_SVW/CNCNSSMTRR_polygon.shp")
str(bc.metro) # check this
  # DOminant Farm Type by CCS Region:
bc.dom.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Dominant Farm Type by CCS/Dominant Farm Types by CCS.shp")
str(bc.dom.farms)
  # Total Farm Count by CCS Region:
bc.total.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Total Farm Type by CCS/Total Farm Count by CCS.shp")
str(bc.total.farms)
  # BC CCS Regions:
bc.ccs<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census subdivs/BC CCS.shp")
str(bc.ccs)

  # SOI Raster for rasterizing later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km buffer raster


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

farms.reproj <- st_make_valid(bc.dom.farms) %>% 
  st_transform(crs=crs(soi.rast))
total.farms.reproj <- st_make_valid(bc.total.farms) %>% 
  st_transform(crs=crs(soi.rast))
soi.bound.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(soi.rast))

# Check to see if they match:
st_crs(bears.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(pres.abs.reproj) == st_crs(bears.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(total.farms.reproj) # [TRUE]

############################ Adding the Distance Variables to the Data:

# Prep Variable 1: Dist to PA's -------------------------------------------
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
bears.reproj$ds__PA_<-min.dist.warp   # Adjust to match col name 
head(bears.reproj)

pres.abs.reproj$ds__PA_<-min.dist.presabs
head(pres.abs.reproj)

  # Remove the units from the values (note: in meters)
as.numeric(bears.reproj$ds__PA_)
as.numeric(pres.abs.reproj$ds__PA_)

  # Convert units from meters to km:
dist_in_km_w<-conv_unit(bears.reproj$ds__PA_,"m","km")
str(dist_in_km_w)
bears.reproj$ds__PA_<-dist_in_km_w
str(bears.reproj) #check data

dist_in_km_p<-conv_unit(pres.abs.reproj$ds__PA_,"m","km")
str(dist_in_km_p)
pres.abs.reproj$ds__PA_<-dist_in_km_p
str(pres.abs.reproj) # check data

# Now each df has our Dist to PA column added in

# Prep Variable 2: Dist to Metro Areas ------------------------------------
  #Calculation of the distance between the metro areas and our points

  # Do this for our WARP only data:
dist.pts2met.warp <- st_distance(bears.reproj, metro.reproj)
head(dist.pts2met.warp)

  # And the pres-abs data:
dist.pts2met.presabs <- st_distance(pres.abs.reproj, metro.reproj)
head(dist.pts2met.pres.abs)

  # Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.met.warp <- apply(dist.pts2met.warp, 1, min)
min.dist.met.presabs <- apply(dist.pts2met.presabs, 1, min)

  # Add Distance Variable into Data table
bears.reproj$dstn___<-min.dist.met.warp
head(bears.reproj)

pres.abs.reproj$dstn___<-min.dist.met.presabs
head(pres.abs.reproj)

  # Remove the units from the values (note: in meters)
as.numeric(bears.reproj$dstn___)  # Make sure col name matches data
as.numeric(pres.abs.reproj$dstn___)

  # Convert units from meters to km:
bears.reproj$dstn___<-conv_unit(bears.reproj$dstn___,"m","km")
head(bears.reproj)

pres.abs.reproj$dstn___<-conv_unit(pres.abs.reproj$dstn___,"m","km")
head(pres.abs.reproj)

  # This added the dist to metro areas column to our data


# Crop Our Data to the SOI Boundary: --------------------------------------

bears.reproj.c <- st_crop(bears.reproj, soi.bound.reproj) # not soi.10k.boundary - right?
pres.abs.reproj.c <- st_crop(pres.abs.reproj, soi.bound.reproj)

  # Plot to see how these look
plot(st_geometry(bears.reproj.c))
plot(st_geometry(pres.abs.reproj.c))

# Check for NA's quick:
which(is.na(bears.reproj.c$ds__PA_))
which(is.na(bears.reproj.c$dstn___))

which(is.na(pres.abs.reproj.c$ds__PA_))
which(is.na(pres.abs.reproj.c$dstn___))


############################## Adding the Agriculture Predictors to Our Data:

# Rasterize Farm Data & WARP Points ---------------------------------------
  ## Here we make rasters for the farm type categories within our SOI region:

  # Crop our Farm Polygons to SOI boundary:
farm.type.soi.crop <- st_intersection(farms.reproj, soi.bound.reproj)
plot(st_geometry(farm.type.soi.crop))

tot.farms.soi.crop <- st_intersection(total.farms.reproj, soi.bound.reproj)
plot(st_geometry(tot.farms.soi.crop))
plot(st_geometry(soi.bound.reproj), add=TRUE) # This works

  # Isolate Farm Type Subsets:
beef.cattle <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Beef cattle ranching and farming, including feedlots [112110]") 
cattle.ranch <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Cattle ranching and farming [1121]") 
other.animal <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Other animal production [1129]") 
fruit.tree.nut <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Fruit and tree nut farming [1113]") 
other.crop <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Other crop farming [1119]") 
greenhouse <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Greenhouse, nursery and floriculture production [1114]") 
veg.melon <- dplyr::filter(farm.type.soi.crop, N_A_I_C == "Vegetable and melon farming [1112]") 

  # Make farm type a spatvector:
farm.type.soi.sv <- vect(farm.type.soi.crop)

  # And our sub categories as spat vectors:
beef.cat.sv <- vect(beef.cattle)
cat.ranch.sv <- vect(cattle.ranch)
other.animal.sv <- vect(other.animal)
fruit.tree.sv <- vect(fruit.tree.nut)
other.crop.sv <- vect(other.crop)
greenhouse.sv <- vect(greenhouse)
veg.mel.sv <- vect(veg.melon)

  # Now farm count:
farm.count.soi.sv <- vect(tot.farms.soi.crop)
plot(farm.count.soi.sv)

  # Rasterize the farm type and count:
farm.type.rast <- terra::rasterize(farm.type.soi.sv, soi.rast, field = "N_A_I_C")
plot(farm.type.rast)

farm.count.rast <- terra::rasterize(farm.count.soi.sv, soi.rast, field = "VALUE")
plot(farm.count.rast)

  # And our subset rasters:
beef.cattle.rast <- terra::rasterize(beef.cat.sv, soi.rast)
cattle.ranch.rast <- terra::rasterize(cat.ranch.sv, soi.rast, field = "N_A_I_C")
other.animal.rast <- terra::rasterize(other.animal.sv, soi.rast, field = "N_A_I_C")
fruit.treenut.rast <- terra::rasterize(fruit.tree.sv, soi.rast, field = "N_A_I_C")
other.crop.rast <- terra::rasterize(other.crop.sv, soi.rast, field = "N_A_I_C")
greenhouse.rast <- terra::rasterize(greenhouse.sv, soi.rast, field = "N_A_I_C")
veg.mel.rast <- terra::rasterize(veg.mel.sv, soi.rast, field = "N_A_I_C")


  # Fix the column names:
names(farm.type.rast)[names(farm.type.rast) == "N_A_I_C"] <- "Dominant Farm Type by CCS"

names(beef.cattle.rast)[names(beef.cattle.rast) == "N_A_I_C"] <- "Beef cattle ranching and farming, including feedlots [112110]"
names(cattle.ranch.rast)[names(cattle.ranch.rast) == "N_A_I_C"] <- "Cattle ranching and farming [1121]"
names(other.animal.rast)[names(other.animal.rast) == "N_A_I_C"] <- "Other animal production [1129]"
names(fruit.treenut.rast)[names(fruit.treenut.rast) == "N_A_I_C"] <- "Fruit and tree nut farming [1113]"
names(other.crop.rast)[names(other.crop.rast) == "N_A_I_C"] <- "Other crop farming [1119]"
names(greenhouse.rast)[names(greenhouse.rast) == "N_A_I_C"] <- "Greenhouse, nursery and floriculture production [1114]"
names(veg.mel.rast)[names(veg.mel.rast) == "N_A_I_C"] <- "Vegetable and melon farming [1112]" 

names(farm.count.rast)[names(farm.count.rast) == "VALUE"] <- "Total Farm Count by CCS"

  # Merge these back to SOI Rasters (so we have values for the SOI region outside each farm type area):
beef.cattle.soi <- merge(beef.cattle.rast, soi.rast)
beef.cattle.soi[beef.cattle.soi == 0] <- 1
beef.cattle.soi[beef.cattle.soi == 327] <- 0

cattle.ranch.soi <- merge(cattle.ranch.rast, soi.rast)
cattle.ranch.soi[cattle.ranch.soi == 0] <- 1
cattle.ranch.soi[cattle.ranch.soi == 327] <- 0

other.animal.soi <- merge(other.animal.rast, soi.rast)
other.animal.soi[other.animal.soi == 0] <- 1
other.animal.soi[other.animal.soi == 327] <- 0

fruit.treenut.soi <- merge(fruit.treenut.rast, soi.rast)
fruit.treenut.soi[fruit.treenut.soi == 0] <- 1
fruit.treenut.soi[fruit.treenut.soi == 327] <- 0

other.crop.soi <- merge(other.crop.rast, soi.rast)
other.crop.soi[other.crop.soi == 0] <- 1
other.crop.soi[other.crop.soi == 327] <- 0

greenhouse.soi <- merge(greenhouse.rast, soi.rast)
greenhouse.soi[greenhouse.soi == 0] <- 1
greenhouse.soi[greenhouse.soi == 327] <- 0

veg.mel.soi <- merge(veg.mel.rast, soi.rast)
veg.mel.soi[veg.mel.soi == 0] <- 1
veg.mel.soi[veg.mel.soi == 327] <- 0

  # Save these Farm Rasters:
terra::writeRaster(farm.type.rast, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dom_farm_type_raster.tif")
terra::writeRaster(farm.count.rast, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/total_farm_count_raster.tif" )

terra::writeRaster(beef.cattle.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/beef_cattle_raster.tif")
terra::writeRaster(cattle.ranch.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/cattle_ranching_raster.tif")
terra::writeRaster(other.animal.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/other_animal_prod_raster.tif")
terra::writeRaster(fruit.treenut.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/fruit_treenut_raster.tif")
terra::writeRaster(other.crop.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/other_crop_prod_raster.tif")
terra::writeRaster(greenhouse.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/greenhouse_crop_raster.tif")
terra::writeRaster(veg.mel.soi, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/veg_melon_raster.tif")

dom.farm.rast.check <- rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dom_farm_type_raster.tif")


# Buffer WARP Points Before Attributing Farm Values -----------------------
  # Here we buffer the WARP and pres-abs points by 5000m (5km) before extracting the attributes from the farm polygons
bears.buf <- bears.reproj.c %>% 
  st_buffer(., 5000)
plot(st_geometry(bears.buf)) # Check the buffers

pres.abs.buf <- pres.abs.reproj.c %>% 
  st_buffer(., 5000)
plot(st_geometry(pres.abs.buf)) # Check the buffers

  # Make the buffered points spat vectors:
bears.sv.buf <- vect(bears.buf)
pres.abs.sv.buf <- vect(pres.abs.buf)

# Prep Variable 3: the Dominant Ag Type by CCS ----------------------------
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
bears.reproj.c$Dm_Fr_T <- bears.farm.type.ext[,2]
bears.reproj.c$Ttl_F_C <- bears.total.farm.ext[,2]

pres.abs.reproj.c$Dm_Fr_T <- pres.abs.farm.type.ext[,2]
pres.abs.reproj.c$Ttl_F_C <- pres.abs.total.farm.ext[,2]

# Check for NA's quick:
which(is.na(bears.reproj.c$Dm_Fr_T))
which(is.na(bears.reproj.c$Ttl_F_C))

which(is.na(pres.abs.reproj.c$Dm_Fr_T))
which(is.na(pres.abs.reproj.c$Ttl_F_C))



# WARP All Species Master Data Frame --------------------------------------
  # Save the resulting data frames here:
st_write(bears.reproj.c, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.pres.abs.need.CCS.shp")
st_write(pres.abs.reproj.c, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.pres.abs.need.CCS.shp")

