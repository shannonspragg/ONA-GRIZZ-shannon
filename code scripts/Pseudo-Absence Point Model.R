# Pseudo-absence Binary Model: --------------------------------------------
  #### Here we will generate pseudo-absences to represent our 0's for the binary model with bear conflict reports (1's)


# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
#install.packages("dismo")
library(dismo)
library(stars)

# Bring in Data: ----------------------------------------------------------
soi.10k.buf <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")
warp.ccs.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10_ccs.shp")

# Make SOI into Raster: ---------------------------------------------------
soi.rast <- st_rasterize(soi.10k.buf)

# export as tiff:
write_stars(soi.rast, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif")

#bring it in:
soi.rast <- raster("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif")

# Generate Random Points for Pseudo-absences: -----------------------------
set.seed(2345)
p.abs.pts <- randomPoints(soi.rast, 4000)

plot(soi.rast)
plot(p.abs.pts, add=TRUE) # This gives us our absence points!

# Make this a data frame:
abs.pts.df <- data.frame(p.abs.pts)


################## START HERE: need to merge dataframes together despite missing columns

# Make these spatial points:
abs.pts.sf <- st_as_sf(abs.pts.df, coords= c("x","y"), crs= st_crs(warp.ccs.df))

# Add the missing columns:
add.cols <- setdiff(names(warp.ccs.df), names(abs.pts.sf))

add_columns(abs.pts.sf, bears.reps)

abs.pts.sf.b <- cbind(abs.pts.sf, add.cols)

# Let's try this manually:
abs.pts.sf['encontr_d'] <- NA
abs.pts.sf['encntr_dt'] <- NA
abs.pts.sf['spcs_nm'] <- NA
abs.pts.sf['encntr_lc'] <- NA
abs.pts.sf['encntr_dl'] <- 1
abs.pts.sf['encntr_y'] <- 0
abs.pts.sf['encntr_lt'] <- NA
abs.pts.sf['encntr_ln'] <- NA
abs.pts.sf['attrct_'] <- NA
abs.pts.sf['enctyp_'] <- NA
abs.pts.sf['otcm_nm'] <- NA
abs.pts.sf['grop_cd'] <- NA
abs.pts.sf['ttl_ncn'] <- NA
abs.pts.sf['bears'] <- 0
abs.pts.sf['ds__PA_'] <- 0
abs.pts.sf['dstn___'] <- 0
abs.pts.sf['Dm_Fr_T'] <- NA
abs.pts.sf['Ttl_F_C'] <- 0
abs.pts.sf['BphysEx'] <- 0
abs.pts.sf['GrzzInE'] <- 0
abs.pts.sf['BHSExtr'] <- 0
abs.pts.sf['CCSUID'] <- 0
abs.pts.sf['CCSNAME'] <- NA

# Reorder the columns to match:
abs.pts.sf <- abs.pts.sf[ , c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,1)]


# Restructure Dataframe: --------------------------------------------------
  #### Here I need to separate out the bear-only reports as their own data frame, then add the absence points in - probably have to re-extract all of these.

bears.reps <- warp.ccs.df %>% filter(warp.ccs.df$bears == "1")

# Join our bear points with the absence points:

bear.pts.w.abs <- rbind(bears.reps, abs.pts.sf)

# Plot these to check:
plot(st_geometry(bear.pts.w.abs))

# Save as New Df: ---------------------------------------------------------
st_write(bear.pts.w.abs, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_pres.abs.shp")





