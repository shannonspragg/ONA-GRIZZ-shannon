# Create Distance to PA and Metro Variable Rasters: ------------------------------
## Need to use the terra::distance function to take a raster of the SOI PA's and Metro Area's and a spatvector of the pseudo-abs points 
# and calculate the distance for all cell values.


# Load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
#install.packages("fasterize")
library(fasterize)
library(terra)
library(stars)


# Bring in Data: ----------------------------------------------------------

# Our pres-abs dataframe:
warp.pres.abs.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_complete.shp")

# BC PA's (need to crop down to SOI):
bc.PAs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")
str(bc.PAs) # Proper sf object, nice

# BC Metro Areas (also crop down to SOI):
bc.metro<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC census metro areas/CEN_CENSUS_METRO_AREAS_SVW/CNCNSSMTRR_polygon.shp")
str(bc.metro)

# SOI 10km Boundary:
soi.10k.boundary <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km_buf.shp")

# Bring in one of our rasters for rasterizing polygon data later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km


# Reproject All Data ------------------------------------------------------
# Now we have the protected areas projected to match the biophys raster:
warp.pa.reproj <- st_make_valid(warp.pres.abs.df) %>% 
  st_transform(crs=crs(soi.rast))
bc.PAs.reproj <- st_make_valid(bc.PAs) %>% 
  st_transform(crs=crs(soi.rast))
metro.reproj <- st_make_valid(bc.metro) %>% 
  st_transform(crs=crs(soi.rast))
soi.bound.reproj <- st_make_valid(soi.10k.boundary) %>% 
  st_transform(crs=crs(soi.rast))


# Check to see if they match:
st_crs(warp.pa.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(soi.bound.reproj) # [TRUE]

bc.albers.crs <- terra::crs("PROJCRS[\"NAD83 / BC Albers\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"British Columbia Albers\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",45,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-126,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",50,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",58.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",1000000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Province-wide spatial data management.\"],\n        AREA[\"Canada - British Columbia.\"],\n        BBOX[48.25,-139.04,60.01,-114.08]],\n    ID[\"EPSG\",3005]]")

bc.albers <- terra::crs("+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m
+no_defs")


# Crop PA's & Metro to SOI Region: ----------------------------------------
# Crop these points to just BC:
PAs.soi.crop <- st_intersection(bc.PAs.reproj, soi.bound.reproj)
plot(st_geometry(PAs.soi.crop))

metro.soi.crop <- st_intersection(metro.reproj, soi.bound.reproj)
plot(st_geometry(metro.soi.crop))
plot(st_geometry(soi.bound.reproj), add=TRUE) # This works


soi.15km.buf <- soi.bound.reproj %>% 
  st_buffer(., 15000)
plot(st_geometry(soi.15km.buf)) # Check the buffers

soi.pas.15km.buf <- st_intersection(bc.PAs.reproj, soi.15km.buf)


# Filter our PA's:We can filter by sq meters using either the Shap_Ar (in meters) or O_AREA (in hectares) columns - which are shape areas

# Filter these to all bigger than 1 hectare:
soi.PA.filter.1hec <- filter(PAs.soi.crop, O_AREA > 1)
unique(bc.PA.filter.1hec$SUBS_RI)

# 5 hectares:
soi.PA.filter.5hec <- filter(PAs.soi.crop, O_AREA > 5)
# 8 Hectares:
soi.PA.filter.8hec <- filter(PAs.soi.crop, O_AREA > 8)
# 10 hectares:
soi.PA.filter.10hec <- filter(PAs.soi.crop, O_AREA > 10)
# 50 hectares
soi.PA.filter.50hec <- filter(PAs.soi.crop, O_AREA > 50)

# For General Conflict, let's do 1000 hectares:
soi.PA.filter.1000hec <- filter(PAs.soi.crop, O_AREA > 1000)

# For bears conflict, let's do 10,000 ha:
soi.PA.filter.10khec <- filter(PAs.soi.crop, O_AREA > 10000)

#### Filter our 15km Buffered PA's:
soi.15km.buf.1000hec <- filter(soi.pas.15km.buf, O_AREA > 1000) # Our 1k for general species
soi.15km.buf.10khec <- filter(soi.pas.15km.buf, O_AREA > 10000) # Our 10k for bears only



# Save these PA's and Metro Areas for SOI:
st_write(PAs.soi.crop, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI PAs & Metro Areas/soi.PAs.10km.buf.shp")
st_write(metro.soi.crop, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI PAs & Metro Areas/soi.metro.10km.buf.shp")

st_write(soi.15km.buf.1000hec, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI PAs & Metro Areas/1k_ha_soi.PAs.15km.buf.shp")
st_write(soi.15km.buf.10khec, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI PAs & Metro Areas/10k_ha_soi.PAs.15km.buf.shp")


# Rasterize our Points & Polygons: ----------------------------------------

# Make our data spatvectors:
PA.soi.sv <- vect(PAs.soi.crop)

metro.soi.sv <- vect(metro.soi.crop)
plot(metro.soi.sv)

warp.ps.sv <- vect(warp.pa.reproj)

# Our filtered PA's:

PA.soi.1kha <- vect(soi.PA.filter.1000hec) # Our > 1000 ha PA's
PA.soi.10kha <- vect(soi.PA.filter.10khec) # Our > 10k ha PA's

# Create a Continuous Raster for Cell Distance to PA's: -------------------

dist.pa.raster <- terra::distance(soi.rast, PA.soi.sv)

dist.met.raster <- terra::distance(soi.rast, metro.soi.sv)


# Do this for our filtered PA's:
dist.pa.raster.1kha <- terra::distance(soi.rast, PA.soi.1kha) # Dist for our 1k PA's

dist.pa.raster.10kha <- terra::distance(soi.rast, PA.soi.10kha) # Dist for our 10k PA's


# Calculate Distance to PA's & Metro from points: SKIP THIS -------------------------

dist.pa.matrix <- terra::distance(warp.ps.sv, PA.soi.sv)

dist.met.matrix <- terra::distance(warp.ps.sv, metro.soi.sv)

# Add Distance Variable into Data table

warp.ps.sv$dist2PA<-dist.pa.matrix
head(warp.ps.sv)

warp.ps.sv$dist2Metro <- dist.met.matrix

# Remove the units from the values (note: in meters)
as.numeric(warp.ps.sv$dist2PA)
as.numeric(warp.ps.sv$dist2Metro)

# Convert units from meters to km:
library(measurements)
warp.ps.sv$dist2PA<-conv_unit(warp.ps.sv$dist2PA,"m","km")

warp.ps.sv$dist2Metro <- conv_unit(warp.ps.sv$dist2Metro, "m", "km")

head(warp.ps.sv) # Perfect!


# Make Metro and PA Dist to Points Rasters SKIP THIS: -----------------------------------------

#pa.soi.rast <- terra::rasterize(warp.ps.sv, soi.rast, field = "dist2PA")

#metro.soi.rast <- terra::rasterize(warp.ps.sv, soi.rast, field = "dist2Metro")



# Check our Rasters: ------------------------------------------------------

# Check this to see if it looks right:
plot(dist.pa.raster.1kha)
plot(warp.pa.reproj, add=TRUE) #YASS

plot(dist.pa.raster.10kha) # Plot our 10k ha for bears
plot(dist.pa.raster.1kha)  # Plot our 1k ha for general conflict

# Fix the column names:
names(dist.pa.raster)[names(dist.pa.raster) == "SOI_10km"] <- "Distance to Nearest PA (km)"


# Make sure our rasters are in km:
dist.pa.rast.1kha <- conv_unit(dist.pa.raster.1kha,"m","km") # There we go
dist.pa.rast.10kha <- conv_unit(dist.pa.raster.10kha,"m","km")
dist.met.raster <- conv_unit(dist.met.raster,"m","km")

names(dist.pa.rast.1kha)[names(dist.pa.rast.1kha) == "SOI_10km"] <- "Distance to Nearest PA (km)"
names(dist.pa.rast.10kha)[names(dist.pa.rast.10kha) == "SOI_10km"] <- "Distance to Nearest PA (km)"
names(dist.met.raster)[names(dist.met.raster) == "SOI_10km"] <- "Distance to Nearest Metro (km)"




# Save our Rasters: -------------------------------------------------------
terra::writeRaster(dist.pa.raster, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dist2PA_raster.tif")
terra::writeRaster(dist.met.raster, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dist2metro_raster.tif" )

terra::writeRaster(dist.pa.rast.1kha, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/1k_ha_dist2pa_raster.tif" )
terra::writeRaster(dist.pa.rast.10kha, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/10k_hadist2pa_raster.tif" )
