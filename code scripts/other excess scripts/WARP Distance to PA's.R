# WARP Distance to PA's Variable ------------------------------------------
# Here we will take part of the script from WARP Data Manipulation to run a 
# Distance to PA variable for the bears only full dataset

# Bring in the WARP Bear Data ---------------------------------------------
# We were smart and saved this in the WARP Ag Overlay script, import here:
bears.sf.full <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Only Full.shp")
str(bears.sf.full) 
head(bears.sf.full) # This looks good, reads in as an sf data.frame!

### Bring in our completed pseudo-abs and warp df to replace our PA variables:

# Here we have our presence-absence data:
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_varint_means.shp")
# The WARP SOI Data:
warp.soi <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP SOI - All Reports/updated.warp.soi.df.shp")


# Import PA Data ----------------------------------------------------------
# Let's bring this in - saved from the previous WARP Data Manipulation script:
bc.ab.PA<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/bc_ab_PAs.shp")
str(bc.ab.PA) # a nice sf dataframe

# This should have been done earlier, but let's crop this to just BC and save it:
bc.PAs <- bc.ab.PA %>% 
  filter(., LOC_E == "British Columbia") %>% 
  st_make_valid()
st_write(bc.PAs, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC protected areas/BC PAs.shp")

##### Bring in our updates PA data with SOI 15km buffer
general.con.pas <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI PAs & Metro Areas/1k_ha_soi.PAs.15km.buf.shp")
bear.con.pas <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI PAs & Metro Areas/10k_ha_soi.PAs.15km.buf.shp")


# Re-project the Data ------------------------------------------------------
albers.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83
# +units=m +no_defs")
st_crs(albers.crs) # Let's Match the data.frames to this CRS
bc.albers.crs <- crs("PROJCRS[\"NAD83 / BC Albers\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"British Columbia Albers\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",45,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-126,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",50,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",58.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",1000000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Province-wide spatial data management.\"],\n        AREA[\"Canada - British Columbia.\"],\n        BBOX[48.25,-139.04,60.01,-114.08]],\n    ID[\"EPSG\",3005]]")

# Now we have the protected areas projected to match the bears data
bears.reproj <- st_transform(bears.sf.full, st_crs(albers.crs))
 plot(st_geometry(bears.reproj))
bc.PAs.reproj <- st_transform(bc.PAs, st_crs(albers.crs))
plot(st_geometry(bc.PAs.reproj))

# Check our PA's and new data:
st_crs(warp.soi) == st_crs(general.con.pas) # [TRUE] = These ARE the same
st_crs(general.con.pas) == st_crs(bear.con.pas) # [TRUE] = These ARE  the same
st_crs(warp.pres.abs) == st_crs(warp.soi) # [TRUE] = These ARE the same


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

### Do this for our Two DF's and new PA's:
# General Conflict:
dist.pas.gen <- st_distance(warp.pres.abs, general.con.pas)
head(dist.pas.gen)

# Bear Conflict Data:
dist.pas.bear <- st_distance(warp.soi, bear.con.pas)
head(dist.pas.bear)

# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist.g <- apply(dist.pas.gen, 1, min)
str(min.dist.g)

min.dist.b <- apply(dist.pas.bear, 1, min)
str(min.dist.b)


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

### Do this for our general and bear df's:
warp.pres.abs$ds__PA_<-min.dist.g
head(warp.pres.abs)

warp.soi$ds__PA_ <- min.dist.b
head(warp.soi)

# Remove the units from the values (note: in meters)
as.numeric(warp.pres.abs$ds__PA_)
as.numeric(warp.soi$ds__PA_)

# Convert units from meters to km:
install.packages("measurements")
library(measurements)

warp.pres.abs$ds__PA_ <- conv_unit(warp.pres.abs$ds__PA_,"m","km")
warp.soi$ds__PA_ <- conv_unit(warp.soi$ds__PA_,"m","km")

head(warp.pres.abs)
head(warp.soi)


# Save our Data: ----------------------------------------------------------

st_write(bears.reproj,"/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Dist to PAs.shp")

### Save these updated DF's:
st_write(warp.pres.abs,"/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/WARP Final Pres-abs/pres_abs_1k_pa.shp")
st_write(warp.soi,"/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP SOI - All Reports/warp_final_10k_pa.shp")




