# Creating our CCS Raster: Varying Intercept for CCS Region: --------------
    ## Here we need to produce a raster where each CCS has the median posterior estimate of the 
#   mean of the random effect for that CCS. We can do so by making our CCS regions into a raster, saving the posterior mean estimates 
#   of the model with the random effect to our points df, making the points$posteriormean into a raster, and then extracting those
#   point values to each CCS region, then saving that as a raster.



# Load Packages: ----------------------------------------------------------
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


# Bring in Data: ----------------------------------------------------------

soi.ccs.crop <- st_read( "/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI CCS regions/SOI_CCS_10km.shp")

warp.pres.abs.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/WARP Final Pres-abs/pres_abs_1k_pa.shp")

# Bring in one of our rasters for rasterizing polygon data later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/SOI Ecoprovince Boundary/SOI_10km.tif") # SOI Region 10km


# Reproject All Data ------------------------------------------------------
# Now we have the protected areas projected to match the biophys raster:
warp.pa.reproj <- st_make_valid(warp.pres.abs.df) %>% 
  st_transform(crs=crs(soi.rast))
soi.ccs.reproj <- st_make_valid(soi.ccs.crop) %>% 
  st_transform(crs=crs(soi.rast))

# Check to see if they match:
st_crs(warp.pa.reproj) == st_crs(soi.ccs.reproj) # [TRUE] = These ARE now the same


# Posterior Distribution (for running script): ----------------------------

# Scale the Variables: ---
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

b2pa.dist.ps <- scale2sd(warp.pres.abs$ds__PA_)
total.farms.ps <- scale2sd(warp.pres.abs$Ttl_F_C)
b2met.dist.ps <- scale2sd(warp.pres.abs$dstn___)
grizzinc.ps <- scale2sd(warp.pres.abs$GrzzInE)
bhs.ps <- scale2sd(warp.pres.abs$BHSExtr)
biophys.ps <- scale2sd(warp.pres.abs$BphysEx)
pop.dens <- scale2sd(warp.pres.abs$Human_Dens)

bears_presence_ps <- warp.pres.abs$bears # Binomial bears
dom.farms.ps <- warp.pres.abs$Dm_Fr_T # Dominant farm type covariate -- non numeric
dom.farms.ps <- as.factor(warp.pres.abs$Dm_Fr_T) # Making this work as a factor

warp.pres.abs$CCSNAME <- as.factor(warp.pres.abs$CCSNAME)
warp.pres.abs$CCSUID <- as.factor(warp.pres.abs$CCSUID)

CCSUID.ps <- warp.pres.abs$CCSUID
CCSNAME.ps <- warp.pres.abs$CCSNAME

which(is.na(warp.pres.abs$CCSNAME.ps)) # Like 200 NA's!!
which(is.na(warp.pres.abs$CCSUID.ps)) # Like 200 NA's!!

# Add an QUADRATIC term for Farm Count: ---
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq.ps <- total.farms.ps*total.farms.ps
# Fit Model with Rstanarm: ---
# Make a mini data frame with just our predictors (no spatial info):
mini.warp.df.ps <-  data.frame(bears_presence_ps, b2pa.dist.ps, b2met.dist.ps, total.farms.ps, total.farms.ps, dom.farms.ps, grizzinc.ps, bhs.ps, biophys.ps, CCSUID.ps, CCSNAME.ps, pop.dens)

# Make our outcome to be factor type and create x and y variables:
mini.warp.df.ps$bears_presence_ps <- factor(mini.warp.df.ps$bears_presence_ps)

# Set our prior:
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# Add in our other variables:
post.pa.full <- stan_glmer(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.sq.ps + pop.dens + (1 | CCSNAME.ps), 
                           data = mini.warp.df.ps,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           seed = SEED, refresh=0) # we add seed for reproducability


# Extracting Mean Posterior Predictions: ----------------------------------

# Extract our Posterior Estimates from the full model with varying intercept:
preds.ccs.varying <- posterior_epred(post.pa.full)
med.preds <- apply(preds.ccs.varying, 2, median)

# Add this as a column to our data frams:
warp.pres.abs$Post.Med.Preds <- med.preds


# Extracting CCS Varying Intercept from Posterior: ------------------------

# Need to make the ranef(post.pa.full) into a dataframe, join by CCS name to our warp.pres.abs, and then save:
varying.int.means <- as.data.frame(ranef(post.pa.full))
vary.int.subset <- varying.int.means[ , c("grp", "condval")]

# Join the tab data with spatial:  
ccs.varint.join <- merge(soi.ccs.crop, vary.int.subset, by.x = "CCSNAME", by.y = "grp")

# Now that it's spatial, do a spatial join to assign a varying intercept mean to each point:
warp.varint.join <- st_join(warp.pres.abs, left = TRUE, ccs.varint.join) # join points

# Clean this up
warp.varint.join$PRNAME <- NULL
warp.varint.join$PRNTCDVSNC <- NULL
warp.varint.join$PRUID <- NULL
warp.varint.join$CDUID <- NULL
warp.varint.join$CPRVNCCD <- NULL
warp.varint.join$FTRCD <- NULL
warp.varint.join$CPRVNCCD <- NULL
warp.varint.join$PRNTCDVSNC <- NULL
warp.varint.join$FFCTVDT <- NULL
warp.varint.join$XPRDT <- NULL
warp.varint.join$OBJECTID <- NULL
warp.varint.join$AREA_SQM <- NULL
warp.varint.join$FEAT_LEN <- NULL 
warp.varint.join$CDNAME <- NULL
warp.varint.join$CDTYPE <- NULL
warp.varint.join$CPRVNCNM <- NULL
warp.varint.join$CCSNAME.x <- NULL
warp.varint.join$CCSUID.x <- NULL

# Rename these quick:
names(warp.varint.join)[names(warp.varint.join) == "CCSUID.y"] <- "CCSUID"
names(warp.varint.join)[names(warp.varint.join) == "CCSNAME.y"] <- "CCSNAME"
names(warp.varint.join)[names(warp.varint.join) == "condval"] <- "CCS Varying Int Mean"

# Save this as completed data frame:
st_write(warp.varint.join, "/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_varint_means.shp")

warp.varint.join <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_varint_means.shp")
# Make our CCS Raster: ----------------------------------------------------

# Make these spatvectors:
soi.ccs.sv <- vect(soi.ccs.reproj)
plot(soi.ccs.sv)

warp.ps.sv <- vect(warp.varint.join)

# Make our Raster:
soi.ccs.rast <- terra::rasterize(soi.ccs.sv, soi.rast, field = "CCSNAME")


# Make Raster for our Posterior Means for CCS Varying Intercept (SKIP): ---------------------
varint.means.rast <- terra::rasterize(warp.ps.sv, soi.rast, field = "CCSVrIE")

# Fix the column name:
names(varint.means.rast)[names(varint.means.rast) == "SOI_10km"] <- "CCS Varying Intercept Mean Estimate"

# Extract Values of Posterior Means to CCS regions: -

warp.varint.mean.ext <- terra::extract(varint.means.rast, soi.ccs.sv, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
soi.ccs.sv$CCSMean <- warp.varint.mean.ext[,2] 


# Make our CCS Post Mean Raster: ------------------------------------------

ccs.varint.means.rast <- terra::rasterize(soi.ccs.sv, soi.rast, field = "CCSMean")

# Check this:

plot(warp.ps.sv)
plot(ccs.varint.means.rast, add=TRUE) #HECK YEAHHHH


# Save our CCS Post Means Raster: -----------------------------------------

terra::writeRaster(ccs.varint.means.rast, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/CCS_varint_raster.tif" )








