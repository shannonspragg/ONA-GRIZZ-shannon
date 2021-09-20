# Combining all WARP Variables --------------------------------------------
# In this script I will be joining all of the WARP vairables (Distance to PA, Distance to Metro,
# Total Farm #, and Dominant Ag Type) into a master dataframe so that there is a column for each 
# new vairable tied to the WARP point ID's and their spatial location


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(dplyr)

# Importing the WARP variable data.tables ---------------------------------
# Let's bring in the WARP Dist to PA's first:
warp.dist.to.pa.data <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Bears Dist to PAs.shp")
# Bring in Distance to Metro Areas:
warp.dist.to.metro.data <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears / WARP Dist to Metro Areas.shp")

# Bring in Ag Overlay Data: # THESE TWO have 18098 rows... missing some? may cause issue with the join
warp.dominant.ag.data <- st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Dominant Farm Type by CCS/WARP Dominant Farm Type Join.shp")
warp.total.farm.data <- st_read("/Users/shannonspragg/ONA_GRIZZ/Ag census/Ag Census Dominant Farm Type /Total Farm Type by CCS/WARP Total Farm Count Join.shp")


# Checking Projections ----------------------------------------------------
str(warp.dist.to.pa.data)
st_crs(warp.dist.to.metro.data) == st_crs(warp.dist.to.pa.data) # TRUE 
st_crs(warp.dominant.ag.data) == st_crs(warp.total.farm.data) # TRUE
st_crs(warp.dist.to.metro.data) == st_crs(warp.dominant.ag.data) # TRUE

# Convert y Inputs for Join from Spatial to Regular df --------------------
# y should not have class sf - we convert this (only need one set of geometry)
warp.dom.ag.df <- st_drop_geometry(warp.dominant.ag.data)
# this worked. may just need to strip some geometries for the y slots
warp.total.fm.df <- st_drop_geometry(warp.total.farm.data)
str(warp.total.fm.df)
warp.dist.metro.df <- st_drop_geometry(warp.dist.to.metro.data)
str(warp.dist.metro.df)

# Creating the Master Dataframe -------------------------------------------
# First, we join the warp distance to PA df to the Dominant Ag df:
dist.pa.2.dom.ag.join <- warp.dist.to.pa.data %>% 
  left_join(., warp.dom.ag.df[, c("N_A_I_C", "encontr_d")], by = "encontr_d")
str(dist.pa.2.dom.ag.join)

# Next we join the above df to the Total Farms df: 
master.a.to.total.fm.join <- dist.pa.2.dom.ag.join %>% 
  left_join(., warp.total.fm.df[, c("ttlfrmc", "encontr_d")], by = "encontr_d")
str(master.a.to.total.fm.join)

# Lastly, we join the above combined df with our final df: the Dist to Metro:
master.join <- master.a.to.total.fm.join %>% 
  left_join(., warp.dist.metro.df[, c("d__METR", "encontr_d")], by = "encontr_d")
str(master.join)

# The results of this is a sf df with all 18,146 of our WARP ID's, the corresponding encounter info,
# and columns for the distance to PA (km), Dominant Farm Type by CCS, Total Farm Count by CCS, and Distance
# to Metro Areas (km)
plot(st_geometry(master.join)) # Testing the geometry here
