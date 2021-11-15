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

# Reproject All Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the protected areas projected to match the bears data
bears.reproj <- st_transform(bears.sf, st_crs(albers.crs))
bc.PAs.reproj <- st_transform(bc.PAs, st_crs(albers.crs))
metro.reproj <- st_transform(bc.metro, st_crs(albers.crs))
bc.ccs.reproj <- st_transform(bc.ccs, st_crs(albers.crs))
farms.reproj <- st_transform(bc.dom.farms, st_crs(albers.crs))
total.farms.reproj <- st_transform(bc.total.farms, st_crs(albers.crs))
bc.bound.reproj <- st_transform(bc.boundary, st_crs(albers.crs))

# Check to see if they match:
st_crs(bears.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same
st_crs(metro.reproj) == st_crs(bc.ccs.reproj) # [TRUE]

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

# Add Distance Variable into Datatable
# Here we create a new column for Distance to Metro Areas in km
bears.reproj$distance_to_metro_km<-min.dist.met
head(bears.reproj)

# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$distance_to_metro_km)

# Convert units from meters to km:
bears.reproj$distance_to_metro_km<-conv_unit(bears.reproj$distance_to_metro_km,"m","km")
head(bears.reproj)

# This added the dist to metro areas column to our bears.reproj df

# Prep Variable 3: the Dominant Ag Type by CCS ----------------------------
# Spatial Join: WARP Points to Farm Type Polygon Attributes
# For WARP POINTS that fall within CCS REGIONS, adds FARM TYPE ATTRIBUTES, retains ALL pts if left=TRUE, otherwise uses inner_join
st_make_valid(bears.reproj)
st_make_valid(farms.reproj)
warp.farm.join <- st_join(bears.reproj, left = TRUE, farms.reproj["N_A_I_C"]) # join points
bears.reproj$Dominant_Farm_Type <- warp.farm.join$N_A_I_C
head(bears.reproj) # HECK TO THE YES - we successfully assigned points to a farm type category
str(bears.reproj) # This gives us a nice added column to the master sheet


# Checking for NA's: ------------------------------------------------------
which(is.na(bears.reproj$Dominant_Farm_Type)) # We have like 80 NA's -- why?
bears.reproj[10943,]
plot(st_geometry(bc.bound.reproj))
plot(st_geometry(bears.reproj[26968,]), col = "red", add = TRUE)
# These all seem to be in the Vancouver area.. 
which(is.na(bears.reproj$Dominant_Farm_Type))# Make sure this is valid:

overlap.check <- st_intersects(bears.reproj, farms.reproj)

which(is.na(overlap.check)) # There are none that don't spatially overlap..

# Let's try to plot the farm types by color across BC:
plot(farms.reproj) # This gives us all attribute maps, we want NAIC
plot(st_geometry(farms.reproj), col = sf.colors(5, categorical = TRUE), 
     axes = TRUE)
plot(st_geometry(bears.reproj[26968,]), col = "red", add = TRUE)

# NEED TO: figure out why some farm types won't attribut to points... (without doing this manually)




# Prep Variable 4: Total Farm Count ---------------------------------------
# Spatial Join: WARP Points to Total Farm Polygon Attributes:
# For WARP POINTS that fall within CCS REGIONS, adds FARM COUNT ATTRIBUTES (VALUE), retains ALL pts if left=TRUE, otherwise uses inner_join
st_make_valid(total.farms.reproj)
bears.total.farm.join <- st_join(bears.reproj, left = TRUE, total.farms.reproj["VALUE"]) # join points
bears.reproj$Total_Farm_Count <- bears.total.farm.join$VALUE
head(bears.reproj) # HECK TO THE YES - we successfully assigned points to a farm count category

# Checking for NA's: ------------------------------------------------------
which(is.na(bears.reproj$Total_Farm_Count)) # We have like 80 NA's -- why?
# The same ones as above...
# These all seem to be in the Vancouver area.. maybe issue with points intersecting multiple polygons??

# Let's plot the farm counts by color:
plot(total.farms.reproj, max.plot = 23) # We want value, which is 12
plot(st_geometry(total.farms.reproj), col = sf.colors(12, categorical = FALSE), 
     axes = TRUE)



# WARP All Species Master Data Frame --------------------------------------
# Since we did this progressively, our bears.reproj file is the new "all species master", so
# let us write that into a .shp and a .csv here

st_write(bears.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")
write_csv(bears.reproj, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.csv")

warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")
