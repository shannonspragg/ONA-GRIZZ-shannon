v# Creating a Distance to Metropolitan Areas Variable ----------------------
# Here we will work to import the metropolitan areas shapefile data, create a function that
# calculates the minimum distance from WARP points to the nearest metro area, and add this to 
# the Warp data as a "Distance to Metro" column to be used in regression analysis


# Install User Packages ---------------------------------------------------
install.packages("Rcpp")
install.packages("raster")
install.packages("sf")
installed.packages("sp")
install.packages("rgdal")
install.packages("tidyverse")
library("rgdal")
library("sf")
library("sp")
library("raster")
library("tidyverse")
library("dplyr")

# WARP Data Prep ----------------------------------------------------------
# Here we will bring in the full years worth of WARP bears reports

warp.bears.full<-read.csv("/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/Grizzly Project/QGIS Bears/WARP Data/WARP_bears only_3.24.20_3.31.21.csv")
head(warp.bears.full)

# Merge the two encounter columns into one total column
warp.bears.full$total_encounter<-warp.bears.full$encounter_adults + warp.bears.full$encounter_young
head(warp.bears.full)


# Convert selected species to 1's and all others to 0's: Don't need to do this with "bears only"

bears.conflict.full<- warp.bears.full %>% 
 mutate(warp.bears.full, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
 head(bears.conflict.full)

# Steps to Create a Distance to PA variable:

# Making Conflict Data a Spatial Dataframe --------------------------------

bc.sp<-structure(bears.conflict.full,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.sp)
xy<-bc.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

bears.sf <- as(bears.spdf, "sf")

bears.sf <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Only Full.shp")
str(bears.sf.full) 

# Load in Canada Spatial Data ---------------------------------------------
# This is where we bring in the metro areas shapefile

# I found the .shp for BC Metro Areas! Read in Below:
bc.metro.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census metro areas/CEN_CENSUS_METRO_AREAS_SVW/CNCNSSMTRR_polygon.shp")

# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
bc.metro.sf<- as(bc.metro.shp, "sf")
str(bc.metro.shp)
head(bc.metro.shp)

# Re-project the Data ------------------------------------------------------

# Canada_Albers_Equal_Area_Conic: Now we have the metro areas projected to match the bears data
bears.reproj.full <- st_transform(bears.sf, st_crs(bc.metro.sf))
plot(st_geometry(bc.metro.sf))
st_crs(bears.reproj.full)
bc.met.reproj<- st_transform(bc.metro.sf, st_crs(bears.reproj.full))
plot(st_geometry(bc.met.reproj))

# Try this:
bears.reproj.m <- st_transform(bears.sf, st_crs(albers.crs))
plot(st_geometry(bears.reproj.m))
metro.reproj.m <- st_transform(bc.metro.shp, st_crs(albers.crs))
plot(st_geometry(metro.reproj.m))

# Create Distance of Conflict Points to Metro's -----------------------------
#calculation of the distance between the metro areas and our points

# dist[1:9, 1:9] Here are the distances btwn metro areas and points
dist.pts2met <- st_distance(bears.reproj.m, metro.reproj.m)
head(dist.pts2met)

# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.met <- apply(dist.pts2met, 1, min)

# Add Distance Variable into Datatable -----------------------------------
# Here we create a new column for Distance to Metro Areas in km
bears.reproj.m$distance_to_METRO<-min.dist.met
head(bears.reproj.m)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj.m$distance_to_METRO)

# Convert units from meters to km:
install.packages("measurements")
library(measurements)

metro_dist_in_km<-conv_unit(bears.reproj.m$distance_to_METRO,"m","km")
str(metro_dist_in_km)
bears.reproj.m$distance_to_METRO_km <- metro_dist_in_km
bears.reproj.m$distance_to_METRO<-NULL
head(bears.reproj.m)
plot(st_geometry(bears.reproj.m))

st_write(bears.reproj.m, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears / WARP Dist to Metro Areas.shp")

# Bring in the CCS Farm Type to join with Distance to Metro ---------------
bc.farm.ccs.shp<-st_read("/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/Grizzly Project/ONA_GRIZZ_ss/ONA-GRIZZ-SS/farm_type_ccs.shp")
head(bc.farm.ccs.shp)

farm.ccs.reproj<- st_transform(bc.farm.ccs.shp, st_crs(bears.reproj.full))
  
# Spatial Join of Bears to Metro Areas ----------------------------------
# Trying to join the metro areas dataframe with the bears points one

bears.full.join.met <- st_join(bears.reproj.full,bc.met.reproj) #This worked but introduced NA's where the points didn't exactly intersect

# Attempt to join the farm join with the met join??:
bears.met.to.farm.join<- st_join(bears.full.join.met,farm.ccs.reproj) # Something here went very wrong... HELP

# Trying Join with sp not sf ----------------------------------------------
# This allows us to visually see the overlap of polygons and points 
library("sp")
library("rgeos")
bears.full.sp<-as(bears.reproj.full, Class= "Spatial")
str(bears.full.sp)
met.sp<-as(bc.met.reproj, Class= "Spatial")
str(met.sp)

#Create Buffer around spatial points:
bears_met_buf_sp<- gBuffer(bears.full.sp, width = 2000) # create buffer around center

bears_met_buf_intersects <-  gIntersects (bears_met_buf_sp, met.sp, byid=TRUE)

# what kind of object is this?
class(bears_met_buf_intersects)
# matrix or array
bears_met_sel_sp <- met.sp[as.vector(bears_met_buf_intersects),]
# plot
plot (met.sp, border="#aaaaaa")
plot (bears_met_sel_sp, add=T, col="red") 
plot (bears_met_buf_sp, add=T, lwd = 2)
# Now you can see where the points overlap Metro areas! Yay!




