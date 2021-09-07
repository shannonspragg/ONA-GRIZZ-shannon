# Overlaying WARP Points with Ag Type Polygons ----------------------------
# Here we will work to import the Dominant Ag Areas  shapefile data, overlay it with the 
# WARP points, and try to assign each conflict point to a "Dominant" farm type and total regional farm count


# Install User Packages ---------------------------------------------------
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

# Making Conflict Data a Spatial Dataframe --------------------------------

bc.sp<-structure(warp.bears.full,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.sp)
xy<-bc.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

bears.sf <- as(bears.spdf, "sf")

# Load in Canada Spatial Data ---------------------------------------------
# This is where we bring in the metro areas shapefile

# I found the .shp for BC Metro Areas! Read in Below:
bc.dom.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Dominant and Total Farm Types CCS.shp")

# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
bc.dom.farms.sf<- as(bc.dom.farms, "sf")
str(bc.dom.farms.sf)
head(bc.dom.farms.sf)
st_crs(bc.dom.farms.sf)
st_crs(bears.sf)
# Re-project the Data ------------------------------------------------------

# Canada_Albers_Equal_Area_Conic: Now we have the Ag Census areas projected to match the bears data
bears.reproj.farms <- st_transform(bears.sf, st_crs(bc.dom.farms))
plot(st_geometry(bc.dom.farms))
st_crs(bears.reproj.farms)
bc.met.reproj<- st_transform(bc.metro.sf, st_crs(bears.reproj.full))
plot(st_geometry(bc.met.reproj))
