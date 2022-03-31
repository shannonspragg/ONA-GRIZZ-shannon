# Creating a WARP All Species Master Dataframe ----------------------------
# This is going to be a longer script, but hopefully we can condense this efficently 
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

# Bring in the WARP All Speciec 1 Year Data -------------------------------
conflict.data.all<-read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/WARP 3.24.20 to 3.31.21 full .csv")
head(conflict.data.all)

# Bring in our pres abs data frame to get variables for our absences:
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.need.farms.shp")


# Pre-Prep for Fresh WARP Data (skip this): -------------------------------

# Merge the two encounter columns into one total encounter column:
conflict.data.all$total_encounter<-conflict.data.all$encounter_adults + conflict.data.all$encounter_young
head(conflict.data.all)

# Convert selected species to 1's and all others to 0's:

bears.conflict.all<- conflict.data.all %>% 
  mutate(conflict.data.all, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
head(bears.conflict.all)

# Making Conflict Data a Spatial Dataframe:
bc.sp<-structure(bears.conflict.all,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.sp)
xy<-bc.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

bears.sf <- as(bears.spdf, "sf")
str(bears.sf) # This gives us a sf data frame, good!




# Bring in the PA,  Metro,  Ag Type,  and Farm Count Data -----------------

soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")

bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")
str(bc.PAs) # Proper sf object, nice

bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census metro areas/CEN_CENSUS_METRO_AREAS_SVW/CNCNSSMTRR_polygon.shp")
str(bc.metro)

bc.dom.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Dominant Farm Type by CCS/Dominant Farm Types by CCS.shp")
str(bc.dom.farms)

bc.total.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Total Farm Type by CCS/Total Farm Count by CCS.shp")
str(bc.total.farms)

bc.ccs<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census subdivs/BC CCS.shp")
str(bc.ccs)

# Bring in one of our rasters for rasterizing polygon data later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km


# Reproject All Data ------------------------------------------------------
# Now we have the protected areas projected to match the biophys raster:
bears.reproj <- st_make_valid(warp.pres.abs) %>% 
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
st_crs(metro.reproj) == st_crs(total.farms.reproj) # [TRUE]

# Prep Variable 1: Dist to PA's -------------------------------------------
#Calculation of the distance between the PA's and our points
dist.pts2pas <- st_distance(bears.reproj, bc.PAs.reproj)
head(dist.pts2pas)

# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist <- apply(dist.pts2pas, 1, min)
str(min.dist)

# Add Distance Variable into Datatable:
bears.reproj$ds__PA_<-min.dist
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$ds__PA_)

# Convert units from meters to km:
#install.packages("measurements")
library(measurements)

dist_in_km<-conv_unit(bears.reproj$ds__PA_,"m","km")
str(dist_in_km)
bears.reproj$ds__PA_<-dist_in_km
#bears.reproj$distance_to_PAs <- NULL
str(bears.reproj)

# Now bears.reproj has our Dist to PA column added in

# Prep Variable 2: Dist to Metro Areas ------------------------------------
#Calculation of the distance between the metro areas and our points

dist.pts2met <- st_distance(bears.reproj, metro.reproj)
head(dist.pts2met)

# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.met <- apply(dist.pts2met, 1, min)

# Add Distance Variable into Data table
# Here we create a new column for Distance to Metro Areas in km
bears.reproj$dstn___<-min.dist.met
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$dstn___)

# Convert units from meters to km:
bears.reproj$dstn___<-conv_unit(bears.reproj$dstn___,"m","km")
head(bears.reproj)

# This added the dist to metro areas column to our bears.reproj df

# Crop these points to just BC:
bears.reproj.c <- st_crop(bears.reproj, soi.10k.boundary)
plot(st_geometry(bears.reproj))
# Now the dataset can be picked up and used from here:


# Rasterize Farm Data & WARP Points ---------------------------------------

# Crop our Polygons to SOI boundary:
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

# And our sub categories:
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

# Now let's rasterize the farm type and count:
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

# Merge these back to SOI Rasters:
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
# Here we buffer the WARP points by 5000m (5km) before extracting the attributes from the farm polygons
bears.buf <- bears.reproj %>% 
  st_buffer(., 5000)
plot(st_geometry(bears.buf)) # Check the buffers

# Make the buffer points a spat vector:
bears.sv.buf <- vect(bears.buf)

# Prep Variable 3: the Dominant Ag Type by CCS ----------------------------
# Here I will extract the mean values from each raster to the buffered points

bears.farm.type.ext <- terra::extract(farm.type.rast, bears.sv.buf, modal, na.rm = TRUE) 
# This gives us the mean value of each buffered area --> what we want!
bears.total.farm.ext <- terra::extract(farm.count.rast, bears.sv.buf, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
bears.reproj$Dm_Fr_T <- bears.farm.type.ext[,2]
bears.reproj$Ttl_F_C <- bears.total.farm.ext[,2]

# Check for NA's quick:
which(is.na(bears.reproj$Dm_Fr_T))
which(is.na(bears.reproj$Ttl_F_C))

# Checking for NA's: ------------------------------------------------------
which(is.na(bears.reproj$Ttl_F_C)) # Now only 2 NA's - good
warp.all.sp[10711,]
warp.all.sp[17194,]

plot(st_geometry(bc.bound.reproj))
plot(st_geometry(bears.reproj[26968,]), col = "red", add = TRUE)
# These all seem to be in the Vancouver area.. 
which(is.na(warp.all.sp$Dm_Fr_T))# Still just 2 NA's


# Let's try to plot these 2 NA's and check their location:
plot(farms.reproj) # This gives us all attribute maps, we want NAIC
# Plot the first NA:
plot(st_geometry(farms.reproj), col = sf.colors(5, categorical = TRUE), 
     axes = TRUE)
plot(st_geometry(warp.all.sp[10711,]), col = "red", add = TRUE)

# Plot the second NA
plot(st_geometry(farms.reproj), col = sf.colors(5, categorical = TRUE), 
     axes = TRUE)
plot(st_geometry(warp.all.sp[17194,]), col = "red", add = TRUE)

# Both of these show they are actually in the ocean - so geometry is incorrect. We can remove them then :)
warp.all.sp.master <- warp.all.sp[-c(10711, 17194), ]   # notice the -

which(is.na(warp.all.sp.master$Ttl_F_C)) # Now zero NA's
which(is.na(warp.all.sp.master$Dm_Fr_T)) # Now zero NA's!
which(is.na(warp.all.sp.master$ds__PA_)) # no NA's
which(is.na(warp.all.sp.master$dstn___)) # no NA's



# WARP All Species Master Data Frame --------------------------------------
# Since we did this progressively, our bears.reproj file is the new "all species master", so
# let us write that into a .shp here
st_write(bears.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /warp.pres.abs.need.CCS.shp")

# Bring this back in to check it:
warp.all.sp.final <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")

is.na(warp.all.sp.master)

# Creating a Mini DF for Examples: ----------------------------------------
mini.warp.df <- warp.all.sp[1:5000,]
plot(st_geometry(mini.warp.df))
st_write(mini.warp.df, "/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/QGIS Tutorial/example data/mini.warp.df.shp")

st_write(bc.dom.farms,"/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/QGIS Tutorial/example data/bc.farms.shp")

writeRaster(biophys.cum.curmap, "/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/QGIS Tutorial/example data/biophys.curmap.tif")
