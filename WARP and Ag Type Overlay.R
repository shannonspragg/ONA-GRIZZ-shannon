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
str(bears.sf)
# Write this into a .shp for future ease of importing!
st_write(bears.sf, "WARP Bears Only Full.shp") #These map well!


# FUTURE USE: Importing WARP Bears Full .shp ------------------------------
# Here we practice importing it and make sure the data is all there:
bears.full <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Only Full.shp")
str(bears.full)
head(bears.full) # This looks good, reads in as an sf data.frame!
str(bears.full)
# Load in Canada Spatial Data ---------------------------------------------
# This is where we bring in the farm types shapefile

# I found the .shp for BC Metro Areas! Read in Below:
bc.dom.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Dominant Farm Type by CCS/Dominant Farm Types by CCS.shp")

# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
bc.dom.farms.sf<- as(bc.dom.farms, "sf")
str(bc.dom.farms.sf)

# Re-project the Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the Ag Census areas projected to match the bears data
bears.reproj <- st_transform(bears.full, st_crs(albers.crs))
plot(st_geometry(bears.reproj))
farms.reproj <- st_transform(bc.dom.farms.sf, st_crs(albers.crs))
plot(st_geometry(farms.reproj))
str(bears.reproj) # This has all 18146 rows, good

# Check to see if they match:
st_crs(bears.reproj) == st_crs(farms.reproj) # [TRUE] = These ARE now the same

# Spatial Join: WARP Points to Farm Type Polygon Attributes ---------------
# For WARP POINTS that fall within CCS REGIONS, adds FARM TYPE ATTRIBUTES, retains ALL pts if left=TRUE, otherwise uses inner_join
bears.farm.join <- st_join(bears.reproj, left = TRUE, farms.reproj["N_A_I_C"]) # join points
head(bears.farm.join) # HECK TO THE YES - we successfully assigned points to a farm type category
str(bears.farm.join)
which(is.na(bears.farm.join$N_A_I_C))

# Let's Plot this:
plot(farms.reproj$geometry, border="gray20", col=NA)
plot(bears.reproj$geometry, pch=21, cex=0.7, col="purple", bg="gray80", add=T)
# There we go, nice points overlayed with the CCS polygons!

# Write Farm Type Join into .shp: -----------------------------------------
st_write(bears.farm.join, "/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Dominant Farm Type by CCS/WARP Dominant Farm Type Join.shp")

unique(bears.farm.join$N_A_I_C)
tail(names(sort(table(bears.farm.join$N_A_I_C))), decreasing= TRUE, 10) #This gives us top 10 most frequent farm types by report
