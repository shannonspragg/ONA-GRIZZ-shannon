# Cropping WARP Points to BC Ecoprovince ----------------------------------
####### Here I will be bringing in the shapefule for BC ecoprovinces and cropping this to the Southern Interior ecoprovince. I will then crop the
# WARP points down to this shapefile and determine if we have enough bear & other wildlife reports. I will play with a few buffers (10km, 50km) to
# see if these are needed to encapulate a better amount of bear reports.


# Load Packages -----------------------------------------------------------
library(sf)
library(raster)
library(terra)
library(tidyverse)


# Bring in Data: ----------------------------------------------------------
bc.ecoprovs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Ecoprovinces/ERC_ECOPRO_polygon.shp")
warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Master DF (+CS resistance values)/WARP Master DF (+CS resist values).shp")


# Filter down to Southern Interior Ecoprovince: ---------------------------
# Here we select for just the southern interior province
south.interior.ep <- bc.ecoprovs %>% filter(bc.ecoprovs$CPRVNCNM == "SOUTHERN INTERIOR")


# Check Projections: ------------------------------------------------------
st_crs(warp.all.sp) == st_crs(south.interior.ep) #TRUE

# Plot these together to see overlap:
plot(st_geometry(south.interior.ep))
plot(st_geometry(warp.all.sp), add= TRUE)


# Crop WARP Points to Boundary -------------------------------------------- 
  # Now we crop the WARP points to those withiin our southern interior ecoprovince

warp.crop <- st_intersection(warp.all.sp, south.interior.ep) # 5,335 total reports

# Let's see how many bears this has:
bears <- warp.crop %>% filter(warp.crop$spcs_nm == "BLACK BEAR" | warp.crop$spcs_nm == "GRIZZLY BEAR") # 1890 bears out of 5335 total reports
plot(st_geometry(bears))


# Buffering EcoProvince by 10km and 50km: ---------------------------------
  # Now let's butter our ecoprovince and see how many more reports it captures with the buffers:
south.int.10k.buf <- st_buffer(south.interior.ep, 10000)
south.int.50k.buf <- st_buffer(south.interior.ep, 50000)

plot(st_geometry(south.int.10k.buf))
plot(st_geometry(south.interior.ep), add= TRUE) # Here we see it with a 10k buffer

plot(st_geometry(south.int.50k.buf))
plot(st_geometry(south.interior.ep), add= TRUE) # Here we see it with a 50k buffer


# Reports Within a 10k Buffer: --------------------------------------------
  # Let's count how many total and just bear reports we include with a 10k buffer:
warp.crop.10k <- st_intersection(warp.all.sp, south.int.10k.buf) # This gives 5,606 total reports

# Let's see how many bears this has:
bears.10k <- warp.crop.10k %>% filter(warp.crop.10k$spcs_nm == "BLACK BEAR" | warp.crop.10k$spcs_nm == "GRIZZLY BEAR") # 2,062 bears out of 5,606 total reports



# Reports Within a 50k Buffer: --------------------------------------------
# Let's count how many total and just bear reports we include with a 10k buffer:
warp.crop.50k <- st_intersection(warp.all.sp, south.int.50k.buf) # This gives 7,141 total reports

# Let's see how many bears this has:
bears.50k <- warp.crop.50k %>% filter(warp.crop.50k$spcs_nm == "BLACK BEAR" | warp.crop.50k$spcs_nm == "GRIZZLY BEAR") # 3,211 bears out of 7,141 total reports

# Plot our points within the buffer:
plot(st_geometry(south.interior.ep))
#plot(st_geometry(south.int.50k.buf, add= TRUE))
plot(st_geometry(warp.crop.50k), add= TRUE) # Here we see them within a 50k buffer


