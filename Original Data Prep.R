# Predictor Data Prep -----------------------------------------------------
  ## Here we load in the original data for our WARP conflict reports, BC Ecoprovinces, CAN census regions, CAN census Data, and protected areas.
  # Author note -- (this is combining Cropping WARP to Ecoprov, WARP Dom Farm Type, WARP Total Farm Count, and WARP Dist top PAs (the very beginning))

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(fasterize)
library(terra)
library(stars)

# Bring in the Data -------------------------------
  # WARP All Species 1 Year:
warp.all.sp <-read.csv("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/WARP 3.24.20 to 3.31.21 full .csv")
  # BC Ecoprovinces:
bc.ecoprovs <- st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Ecoprovinces/ERC_ECOPRO_polygon.shp")
  # CAN Agriculture Data
farm.type <- read.csv("/Users/shannonspragg/ONA_GRIZZ/Ag census/farm type_32100403/farm type_32100403.csv")
  # CAN Consolidated Census Subdivisions (CCS):
can.ccs.shp<-st_read("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CAN census cons subdivisions (CCS)/lccs000b16a_e.shp")


################# We begin by filtering to our SOI ecoprovince, buffering, and cropping our conflict data to the buffered region:

# Filter down to Southern Interior Ecoprovince: ---------------------------
  # Here we select for just the southern interior province
south.interior.ep <- bc.ecoprovs %>% filter(bc.ecoprovs$CPRVNCNM == "SOUTHERN INTERIOR")

  # Write this for later use:
#st_write(south.interior.ep, "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/BC Ecoprovinces/south.interior.shp")


# Check Projections: ------------------------------------------------------
st_crs(warp.all.sp) == st_crs(south.interior.ep) #TRUE

  # Plot these together to see overlap:
plot(st_geometry(south.interior.ep))
plot(st_geometry(warp.all.sp), add= TRUE)

# Crop WARP Points to SOI Boundary -------------------------------------------- 
  # Now we crop the WARP points to those within our southern interior ecoprovince

warp.crop <- st_intersection(warp.all.sp, south.interior.ep) # 5,335 total reports

  # Let's see how many bears reports this has:
warp.crop %>% filter(warp.crop$spcs_nm == "BLACK BEAR" | warp.crop$spcs_nm == "GRIZZLY BEAR") # 1890 bears out of 5335 total reports


# Buffering EcoProvince by 10km: ---------------------------------
  # Now let's butter our ecoprovince and see how many more reports it captures with the buffers:
south.int.10k.buf <- st_buffer(south.interior.ep, 10000)

plot(st_geometry(south.int.10k.buf))
plot(st_geometry(south.interior.ep), add= TRUE) # Here we see it with a 10k buffer

# Reports Within a 10k Buffer: --------------------------------------------
  # Let's check how many total and just bear reports we include with a 10k buffer:
warp.crop.10k <- st_intersection(warp.all.sp, south.int.10k.buf) # This gives 5,606 total reports

  # Let's see how many bears this has:
warp.crop.10k %>% filter(warp.crop.10k$spcs_nm == "BLACK BEAR" | warp.crop.10k$spcs_nm == "GRIZZLY BEAR") # 2,062 bears out of 5,606 total reports
  # This buffer includes a better sample size for bears and total reports 

# Remove Extra Columns: ---------------------------------------------------
  # Let's remove the unwanted columns from our data frame:
warp.crop.10k$CPRVNCCD <- NULL
warp.crop.10k$FTRCD <- NULL
warp.crop.10k$PRNTCDVSNC <- NULL
warp.crop.10k$FFCTVDT <- NULL
warp.crop.10k$CPRVNCNM <- NULL
warp.crop.10k$XPRDT <- NULL
warp.crop.10k$OBJECTID <- NULL
warp.crop.10k$AREA_SQM <- NULL
warp.crop.10k$FEAT_LEN <- NULL

# Save our Cropped WARP DF ------------------------------------------------
st_write(warp.crop.10k, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10km_buf.shp")


####################### Now, we will filter the CCS regions and Agriculture Data to BC:


# Filter CCS and Ag Files to BC Only ---------------------------------------------------
  # Make sf and filter down to only British Columbia for Census SubDivs (CCS):
can.ccs.sf<- as(can.ccs.shp, "sf")
unique(can.ccs.sf$PRNAME) # Shows that the name for BC is "British Columbia / Colombie-Britannique"
  
# Filter down to just BC:
bc.ccs<-can.ccs.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_make_valid()
  
# Save this for later:
st_write(bc.ccs, "BC CCS.shp")

# Filter the Ag Files down to just BC districts:
  # See here: https://www.statology.org/filter-rows-that-contain-string-dplyr/  searched: 'Return rows with partial string, filter dplyr'
farm.type.bc <- farm.type %>% filter(grepl("British Columbia", farm.type$GEO)) 

# Filtering to just the BC regions with a CCS number (so we can join to the CCS spatial data):
bc.farm.filter.ccs<-farm.type.bc %>%
  filter(., grepl("*CCS59*", farm.type.bc$GEO))

# Save this for later:
  # write_csv(bc.farm.filter.ccs, "bc_farms_filtered.ccs.csv")

# Check to see what specific farm types exist in BC:
unique(farm.type.bc$North.American.Industry.Classification.System..NAICS.) # There are 43 unique farm types in BC

# Filter for just the 2016 census results (the data had 2011 and 2016):
bc.farm.2016.ccs<-bc.farm.filter.ccs %>%
  filter(., grepl("2016", bc.farm.filter.ccs$REF_DATE)) # Now there are 344 observations


# Editing CCS Code into new column for join -------------------------------
  # Here we separate out the CCS code into new column for join with CCS .shp:
bc.ccs$CCSUID.crop<- str_sub(bc.ccs$CCSUID,-5,-1) # Now we have a matching 6 digits
unique(bc.ccs$CCSUID.crop) #This is a 5 digit code
str(bc.farm.2016.ccs) # CHeck the structure before joining


# Joining the CCS with the Farm Type: -------------------------------------
  # Join the BC CCS with Ag Files:
farm.ccs.join <- merge(bc.farm.2016.ccs, bc.ccs, by.x = "CCSUID.crop", by.y = "CCSUID.crop") 

# Double check that this is the correct structure:
str(farm.ccs.join) # Here we have a farm type data frame with Multipolygon geometry - check!

######################## Next, we prepare the Dominant Farm Type and Total Farm Count by CCS Region:

# Selecting Dominant Ag Types By Region -----------------------------------
  # Here we pull out the top 2 farm count values for each of the CCS codes 
    # https://statisticsglobe.com/select-row-with-maximum-or-minimum-value-in-each-group-in-r

farm.ccs.join$VALUE<- as.integer(farm.ccs.join$VALUE)

  # Pull out the top two values from the join:
top.two.ccs.farm.types<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE)  # YAY! This worked!
str(top.two.ccs.farm.types) # check to see that this worked

# Subset the Farm Join & Print --------------------------------------------
  # Extract Total Farm Counts by CCS: (Do this with the dominant farms too)
total.farms.bc<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE) %>% slice_min(., order_by = "VALUE") # This successfully gives us total farm count by CCS region

# Do this to get our dominant (most frequent) farm types by CCS region:
dominant.farms.bc<- farm.ccs.join %>% group_by(GEO) %>% top_n(2,VALUE) %>% slice_tail() # This gives us the dominant type WITHOUT the total farms 

# Save these as .shp's for later:
st_write(dominant.farms.bc,"Dominant Farm Types by CCS.shp")

st_write(total.farms.bc, "Total Farm Count by CCS.shp") 


################################# Lastly, we will filter the CAN Protected Areas down to BC:

  # Load in our Protected Areas Database:
fgdb <- "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/CPCAD-BDCAPC_Dec2020.gdb"

  # List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

  # Read the feature class for PA shapes
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf") 

  # Filter these down to BC:
bc.PAs <- fc.sf %>% 
  filter(., LOC_E == "British Columbia" | LOC_E == "Alberta") %>% 
  st_make_valid()

  # Save for later:
st_write(bc.PAs,"bc_PAs.shp")

