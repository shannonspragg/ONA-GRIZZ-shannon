# Combining all WARP Variables --------------------------------------------
# In this script I will be joining all of the WARP vairables (Distance to PA, Distance to Metro,
# Total Farm #, and Dominant Ag Type) into a master dataframe so that there is a column for each 
# new vairable tied to the WARP point ID's and their spatial location


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


# Creating the Master Dataframe -------------------------------------------
master.test <- warp.dist.to.pa.data %>% 
  left_join(., warp.dom.ag.df, by = "encontr_d")
# y should not have class sf - should we convert this??
warp.dom.ag.df <- st_drop_geometry(warp.dominant.ag.data)
# this worked. may just need to strip some geometries for the y slots
# ALSO - it seems like there is not an issue with the # of ag data objects, result is still 18,146

# NEED TO: figure out why the 

master.df <- warp.metro %>%
  left_join(., warp.pa, by = "WarpID")

