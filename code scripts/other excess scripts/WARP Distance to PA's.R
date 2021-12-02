# WARP Distance to PA's Variable ------------------------------------------
# Here we will take part of the script from WARP Data Manipulation to run a 
# Distance to PA variable for the bears only full dataset

# Bring in the WARP Bear Data ---------------------------------------------
# We were smart and saved this in the WARP Ag Overlay script, import here:
bears.sf.full <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Only Full.shp")
str(bears.sf.full) 
head(bears.sf.full) # This looks good, reads in as an sf data.frame!


# Import PA Data ----------------------------------------------------------
# Let's bring this in - saved from the previous WARP Data Manipulation script:
bc.ab.PA<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/bc_ab_PAs.shp")
str(bc.ab.PA) # a nice sf dataframe

# This should have been done earlier, but let's crop this to just BC and save it:
bc.PAs <- bc.ab.PA %>% 
  filter(., LOC_E == "British Columbia") %>% 
  st_make_valid()
st_write(bc.PAs, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")

# Re-project the Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS

# Now we have the protected areas projected to match the bears data
bears.reproj <- st_transform(bears.sf.full, st_crs(albers.crs))
 plot(st_geometry(bears.reproj))
bc.PAs.reproj <- st_transform(bc.PAs, st_crs(albers.crs))
plot(st_geometry(bc.PAs.reproj))

# Check to see if they match:
st_crs(bears.reproj) == st_crs(bc.PAs.reproj) # [TRUE] = These ARE now the same

# Create Distance of Conflict Points to P.A's -----------------------------
#calculation of the distance between the PA's and our points

# dist[1:9, 1:9] 
dist.pts2pas <- st_distance(bears.reproj, bc.PAs.reproj)
head(dist.pts2pas)
# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist <- apply(dist.pts2pas, 1, min)
str(min.dist)

# Add Distance Variable into Datatable -----------------------------------

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
head(bears.reproj)
str(bears.reproj)

st_write(bears.reproj,"/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Dist to PAs.shp")


