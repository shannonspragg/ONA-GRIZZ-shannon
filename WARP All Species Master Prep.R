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
install.packages("fasterize")
library(fasterize)
library(terra)
library(stars)

# Bring in the WARP All Speciec 1 Year Data -------------------------------
conflict.data.all<-read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/WARP 3.24.20 to 3.31.21 full .csv")
head(conflict.data.all)

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
bc.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Boundary.shp")

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
biophys.cum.curmap <- rast("/Users/shannonspragg/rasters/biophys_normalized_cum_currmap.tif")


# Reproject All Data ------------------------------------------------------
# Now we have the protected areas projected to match the biophys raster:
bears.reproj <- st_make_valid(bears.sf) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=crs(biophys.cum.curmap))

farms.reproj <- st_make_valid(bc.dom.farms) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
total.farms.reproj <- st_make_valid(bc.total.farms) %>% 
  st_transform(crs=crs(biophys.cum.curmap))
bc.bound.reproj <- st_make_valid(bc.boundary) %>% 
  st_transform(crs=crs(biophys.cum.curmap))


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
bears.reproj$distance_to_PAs<-min.dist
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$distance_to_PAs)

# Convert units from meters to km:
install.packages("measurements")
library(measurements)

dist_in_km<-conv_unit(bears.reproj$distance_to_PAs,"m","km")
str(dist_in_km)
bears.reproj$distance_to_PA_km<-dist_in_km
bears.reproj$distance_to_PAs <- NULL
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
bears.reproj$distance_to_metro_km<-min.dist.met
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$distance_to_metro_km)

# Convert units from meters to km:
bears.reproj$distance_to_metro_km<-conv_unit(bears.reproj$distance_to_metro_km,"m","km")
head(bears.reproj)

# This added the dist to metro areas column to our bears.reproj df

# Crop these points to just BC:
bears.reproj <- st_crop(bears.reproj, bc.bound.reproj)
plot(st_geometry(bears.reproj))
# Now the dataset can be picked up and used from here:

# NEED TO: rasterize these points, buffer them, then use extract with the MODE (for categorical), to get a value for each point


# Rasterize Farm Data & WARP Points ---------------------------------------

# Make farm type a spatvector:
farm.type.sv <- vect(farms.reproj)

# Now do this for total farms:
plot(total.farms.reproj, max.plot = 23)
total.farms.reproj$VALUE <- as.numeric(total.farms.reproj$VALUE) # Making this numeric

farm.count.sv <- vect(total.farms.reproj)
plot(farm.count.sv)
# Now let's rasterize the farm type and count:
farm.type.rast <- terra::rasterize(farm.type.sv, biophys.cum.curmap, field = "N_A_I_C")
plot(farm.type.rast)

farm.count.rast <- terra::rasterize(farm.count.sv, biophys.cum.curmap, field = "VALUE")
plot(farm.count.rast)

# Buffer WARP Points Before Attributing Farm Values -----------------------
# Here we buffer the WARP points by 500m before extracting the attributes from the farm polygons
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
bears.reproj$Dom_Farm_Type <- bears.farm.type.ext[,2]
bears.reproj$Total_Farm_Count <- bears.total.farm.ext[,2]

# WARP All Species Master Data Frame --------------------------------------
# Since we did this progressively, our bears.reproj file is the new "all species master", so
# let us write that into a .shp and a .csv here

st_write(bears.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")
write_csv(bears.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.csv")

warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")

# Spatial Join -- Doesn't Work (need Raster) ------------------------------
# Spatial Join: WARP Points to Farm Type Polygon Attributes
# For WARP POINTS that fall within CCS REGIONS, adds FARM TYPE ATTRIBUTES, retains ALL pts if left=TRUE, otherwise uses inner_join
st_make_valid(bears.buf)
st_make_valid(farms.reproj)
warp.farm.join <- st_join(bears.buf, left = TRUE, farms.reproj["N_A_I_C"]) # join points
# With the buffer, we get lots of duplicates.. need to merge these
warp.farm.merged <- warp.farm.join %>%
  distinct(encontr_d, .keep_all = TRUE) # Here we just drop the duplicates

bears.reproj$Dominant_Farm_Type <- warp.farm.join$N_A_I_C
head(bears.reproj) # HECK TO THE YES - we successfully assigned points to a farm type category
str(bears.reproj) # This gives us a nice added column to the master sheet


# Checking for NA's: ------------------------------------------------------
which(is.na(bears.reproj$Total_Farm_Count)) # Now only 2 NA's - good
bears.reproj[10943,]
plot(st_geometry(bc.bound.reproj))
plot(st_geometry(bears.reproj[26968,]), col = "red", add = TRUE)
# These all seem to be in the Vancouver area.. 
which(is.na(bears.reproj$Dominant_Farm_Type))# Still just 2 NA's

overlap.check <- st_intersects(bears.reproj, farms.reproj)

which(is.na(overlap.check)) # There are none that don't spatially overlap..

# Let's try to plot the farm types by color across BC:
plot(farms.reproj) # This gives us all attribute maps, we want NAIC
plot(st_geometry(farms.reproj), col = sf.colors(5, categorical = TRUE), 
     axes = TRUE)
plot(st_geometry(bears.reproj[26968,]), col = "red", add = TRUE)


# Creating a Mini DF for Examples: ----------------------------------------
mini.warp.df <- warp.all.sp[1:5000,]
plot(st_geometry(mini.warp.df))
st_write(mini.warp.df, "/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/QGIS Tutorial/example data/mini.warp.df.shp")

st_write(bc.dom.farms,"/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/QGIS Tutorial/example data/bc.farms.shp")

writeRaster(biophys.cum.curmap, "/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/QGIS Tutorial/example data/biophys.curmap.tif")
