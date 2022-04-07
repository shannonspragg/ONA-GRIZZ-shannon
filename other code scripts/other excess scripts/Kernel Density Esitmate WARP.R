# Creating Kernel Density for WARP Points ---------------------------------
# This is my attempt to create a kernel density output for the WARP data (based on what
# we did in our r spatial class -- script to follow)

# Kernel Density & Point Selection Script: ----------------------------------------------------

# Load the packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(geosphere)
library(spatstat)

# Here's What We Need for This: -------------------------------------------
# I need to import a points file (WARP points), a polygon of the BC boundary line,
# and then a raster of grizzly density (from Clayton's data):

# NOTE TO SHANNON: Since the other script is running, we are just gonna fill in the import files
# before runing them

# Download WARP Point Files ----------------------------------------------------

warp.bears.pts<- read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP_bears only_3.24.20_3.31.21.csv")
# Next this needs to be made into an sf object

head(warp.bears.pts)

# Merge the two encounter columns into one total column
warp.bears.pts$total_encounter<-warp.bears.pts$encounter_adults + warp.bears.pts$encounter_young
head(warp.bears.pts)

# Convert selected species to 1's and all others to 0's: Don't need to do this with "bears only"

#bear.conflict.pts<- warp.bears.pts %>% 
#  mutate(warp.bears.pts, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
#head(bears.conflict.pts)

# Steps to Create a Distance to PA variable:

# Making Conflict Data a Spatial Dataframe --------------------------------

bc.pts.sp<-structure(warp.bears.pts,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.pts.sp)
xy<-bc.pts.sp[,c(8,7)]
bears.pts.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.pts.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.pts.spdf)

# bears.pts <- as.ppp(bears.pts.spdf)

# Load a starbucks.shp point feature shapefile
s  <- st_read("starbucks.shp")  
starbucks  <- as.ppp(s)
marks(starbucks) <- NULL
starbucks <- rescale(starbucks, 1000)
Window(starbucks) <- starbucks 


bears.pts.sf <- as(bears.pts.spdf, "sf")

# Convert to spatial points for using geosphere ---------------------------
bears.pts.sp <- as_Spatial(bears.pts.sf)

# Analyzing a point pattern ------------------------------------------------

# I'm loading these packages later so that I don't have to worry about conflicts
library(rgdal)
library(maptools)
library(raster)

# Bring in the BC Boundary Polygon ----------------------------------------

can.bound <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN Province Boundaries/lpr_000b16a_e.shp")

# Make sure it is an sf object
str(can.bound)

bc.boundary<-can.bound %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()
str(bc.boundary)

bcb.spdf <- as_Spatial(bc.boundary)
str(bcb.spdf)


# Import the Grizzly Density Raster  -------------------------------------

grizz.density.raster <- raster("/Users/shannonspragg/ONA_GRIZZ/Grizz Density/Clayton_griz_dens.tiff")
str(grizz.density.raster)
# Reproject and Plot the WARP Points with Polygon -------------------------

# Check the CRS:
st_crs(bears.pts.sf) #WGS84
st_crs(bc.boundary) #Lambert Conformal Conic

# Reproject the BC boundary to WGS 84:
bcb.reproj.sf <- st_transform(bc.boundary, st_crs(bears.pts.sf)) # Both in WGS84
st_crs(bcb.reproj.sf)

# Reproject the Density Raster to WGS84:
new.crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
# grizz.dens.reproj <- st_transform(grizz.density.raster, st_crs(bears.pts.sf)) # Both in WGS84
griz.dens.reproj <- grizz.density.raster %>% st_transform(., new.crs)
# NEED TO: figure out how to reproject a raster!!


# STOPPING POINT ----------------------------------------------------------


# Let's plot these and see:
plot(grizz.density.raster)
plot(st_geometry(bcb.reproj.sf), add=TRUE)
plot(st_geometry(bears.pts.sf), add= TRUE)
#  BEAUTIFUL!

# Window(bears.pts.sf) <- bcb.spdf # This doesnt work

# Not sure if the below is needed:
bc.bo <- as.owin(bcb.spdf)
bc.bo <- as(bcb.spdf, "owin")
??as.owin

# Trying to Do some Analyses Now ---------------------------------------------


# Load a starbucks.shp point feature shapefile
allpoints.sp.adj.proj  <- spTransform(allpoints.sp.adj, proj4string(mn_snbp))
allpoints.sp.adj.proj.ppp    <- as(allpoints.sp.adj.proj, "ppp")

#set marks to null
marks(allpoints.sp.adj.proj.ppp)  <- NULL

#Define the study area
Window(allpoints.sp.adj.proj.ppp) <- snbp
plot(allpoints.sp.adj.proj.ppp, main=NULL, cols=rgb(0,0,0,.2), pch=20)

# Quadrat counts ----------------------------------------------------------
Q <- quadratcount(allpoints.sp.adj.proj.ppp, nx= 5, ny=5)
plot(allpoints.sp.adj.proj.ppp, pch=20, cols="grey70", main=NULL)  # Plot points
plot(Q, add=TRUE) 

# Compute the density for each quadrat
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(allpoints.sp.adj.proj.ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# Counts on a tessellated surface -----------------------------------------

# Load the raster
shooter <- raster(here::here("datatemp/original/shooterRaster/shooterraster/"))
shooterRaster<- raster(here::here("datatemp/original/shooterRaster/shooterraster/"))

# Reclassify raster
brk  <- c( -Inf, 0.5e-04, 0.75e-04, Inf)
shooter.reclass <- cut(shooter, breaks = brk)
shooter.tess <- tess(image=shooter.reclass) #converts the categorical raster into 'quadrats' based on the probability of shooter occurrence
Q   <- quadratcount(allpoints.sp.adj.proj.ppp, tess = shooter.tess)  # Tally counts
Q.d <- intensity(Q)  # Compute density
plot(intensity(Q, image=TRUE), las=1, main=NULL)

# Generate a kernel density estimate --------------------------------------
K1 <- density(allpoints.sp.adj.proj.ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)
K2 <- density(allpoints.sp.adj.proj.ppp, sigma = 10000) # Using a 10km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)
K3 <- density(allpoints.sp.adj.proj.ppp, sigma = 1000) # Using a 1km bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

# Manipulate Density By Changing Sigma and Kernel:
?spatstat::density.ppp
K4 <- density(allpoints.sp.adj.proj.ppp, sigma = 20000,kernel="disc") # Using a 20km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)
K5 <- density(allpoints.sp.adj.proj.ppp, sigma = 2000,kernel="quartic") # Using a 2km bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

# Convert Kernel to Raster:
K3.rast<-raster::raster(K3)
K2.rast<-raster::raster(K2)
K1.rast<-raster::raster(K1)
K4.rast<-raster::raster(K4)
K5.rast<-raster::raster(K5)

comp.stack <- raster::stack(K3.rast,shooter.tess)
raster.cors <- cor(raster::values(comp.stack), use = "pairwise")
plot(raster.cors)


#testing

