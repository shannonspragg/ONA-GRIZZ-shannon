# Overlaying WARP Points with Total Farm Polygons ----------------------------
# Here we will work to import the Total Farm #  shapefile data, overlay it with the 
# WARP points, and try to assign each conflict point to a "Dominant" farm type and total regional farm count


# Install User Packages ---------------------------------------------------
library("rgdal")
library("sf")
library("sp")
library("raster")
library("tidyverse")
library("dplyr")

# Importing WARP Bears Full .shp ------------------------------
# Here we practice importing it and make sure the data is all there:
bears.full <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Only Full.shp")
str(bears.full)
head(bears.full) # This looks good, reads in as an sf data.frame!

# Load in Canada Spatial Data ---------------------------------------------
# This is where we bring in the farm types shapefile

# I found the .shp for BC Metro Areas! Read in Below:
bc.total.farms<-st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Total Farm Type by CCS/Total Farm Count by CCS.shp")

# Make sf and filter down to only British Columbia for Census SubDivs (CCS):
bc.total.farms.sf<- as(bc.dom.farms, "sf")
str(bc.total.farms.sf)

# Re-project the Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the Ag Census areas projected to match the bears data
bears.reproj <- st_transform(bears.full, st_crs(albers.crs))
plot(st_geometry(bears.reproj))
total.farms.reproj <- st_transform(bc.total.farms.sf, st_crs(albers.crs))
plot(st_geometry(total.farms.reproj))
str(bears.reproj)
# Check to see if they match:
st_crs(bears.reproj) == st_crs(total.farms.reproj) # [TRUE] = These ARE now the same

# Spatial Join: WARP Points to Total Farm Polygon Attributes ---------------
# For WARP POINTS that fall within CCS REGIONS, adds FARM COUNT ATTRIBUTES (VALUE), retains ALL pts if left=TRUE, otherwise uses inner_join
bears.total.farm.join <- st_join(bears.reproj, left = TRUE, total.farms.reproj["VALUE",]) # join points
head(bears.total.farm.join) # HECK TO THE YES - we successfully assigned points to a farm type category
names(bears.total.farm.join)[names(bears.total.farm.join) == 'VALUE'] <- 'total farm count' #Just changed the column name here
str(bears.total.farm.join)
# Let's Plot this:
plot(total.farms.reproj$geometry, border="gray20", col=NA)
plot(bears.reproj$geometry, pch=21, cex=0.7, col="purple", bg="gray80", add=T)
# There we go, nice points overlayed with the CCS polygons!

# Write Farm Type Join into .shp: -----------------------------------------
st_write(bears.total.farm.join, "/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Total Farm Type by CCS/WARP Total Farm Count Join.shp")

