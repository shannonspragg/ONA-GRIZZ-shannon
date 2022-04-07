
# General Conflict Regression: --------------------------------------------
    # Just keeping the code from our markdown in here so we can run everything


# Packages: ---------------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(tidyverse)
library(caret)
library(GGally)
library(ggplot2)
library(corrplot)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
library(rstanarm)
options(mc.cores = 1)
library(loo)
library(projpred)
SEED=14124869
library(sjPlot)
library(nloptr)
library(sjmisc)
library(rsq)
library(tidybayes)
library(pROC)
library(bayestestR)





# Load WARP Data: --------------------------------------------------------------
# Here we have our presence-absence data:
warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/WARP Final Pres-abs/pres_abs_1k_pa.shp")
str(warp.pres.abs)

# Access this via googledrive: In our ONA-GRIZZ folder
# at the link: https://drive.google.com/drive/u/0/folders/17n64UwIiwhGVCwTQX8uc31ybWJ0YIdtv

# The WARP SOI Data: ***STILL NEEDS VARINT VARIABLE ADDED***
warp.soi <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP SOI - All Reports/warp_final_10k_pa.shp")

# Load Raster Data: -------------------------------------------------------
# Here we bring in all of our predictor rasters for the SOI region:
# link to data here:   https://drive.google.com/drive/u/0/folders/1T60Al9vHOF-E7ftKzvzN2CMUyF1agC-l

# Dominant Farm Type by CCS Region:
dom.farms.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dom_farm_type_raster.tif")

# Total Farm Count by CCS Region:
tot.farms.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/total_farm_count_raster.tif" )

# Total Farm Count Squared:
tot.farms.sq.rast <- tot.farms.rast * tot.farms.rast # I am not sure how to do this one...

# Distance to Neartest Protected Area (km):
dist2pa.rast.general.1k <- rast(dist.pa.raster.1kha, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/1k_ha_dist2pa_raster.tif" )
dist2pa.rast.bear.10k <- rast(dist.pa.raster.10kha, "/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/10k_hadist2pa_raster.tif" )

# Distance to Nearest Metro Area (km):
dist2met.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dist2metro_raster.tif" )

# Human Pop Density (by sq km):
hm.dens.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/CAN Spatial Data/Human Pop Density/human_dens_SOI_10km.tif") # SOI Region 10km

# CCS Region ID:
ccs.varint.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/CCS_varint_raster.tif" )
names(ccs.varint.rast)[names(ccs.varint.rast) == "PostMean"] <- "CCS Varying Intercept Mean"

# Subset the Dominant Farm Type Raster into Categories:
cattle.ranching.rast <- dom.farms.rast$`Dominant Farm Type by CCS` == "Cattle ranching and farming [1121]"
names(cattle.ranching.rast)[names(cattle.ranching.rast) == "Dominant Farm Type by CCS"] <- "Cattle Ranching & Farming [1121]"
fruit.tree.nut.rast <- dom.farms.rast$`Dominant Farm Type by CCS` == "Fruit and tree nut farming [1113]"
names(fruit.tree.nut.rast)[names(fruit.tree.nut.rast) == "Dominant Farm Type by CCS"] <- "Fruit & Tree Nut Farming [1113]"
other.animal.rast <- dom.farms.rast$`Dominant Farm Type by CCS` == "Other animal production [1129]"
names(other.animal.rast)[names(other.animal.rast) == "Dominant Farm Type by CCS"] <- "Other Animal Production [1129]"
other.crop.rast <- dom.farms.rast$`Dominant Farm Type by CCS` == "Other crop farming [1119]"
names(other.crop.rast)[names(other.crop.rast) == "Dominant Farm Type by CCS"] <- "Other Crop Farming [1119]"
veg.melon.rast <- dom.farms.rast$`Dominant Farm Type by CCS` == "Vegetable and melon farming [1112]"
names(veg.melon.rast)[names(veg.melon.rast) == "Dominant Farm Type by CCS"] <- "Vegetable & Melon Farming [1112]"


# Scale the Variables: ----------------------------------------------------------
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

b2pa.dist.ps <- scale2sd(warp.pres.abs$ds__PA_)
total.farms.ps <- scale2sd(warp.pres.abs$Ttl_F_C)
b2met.dist.ps <- scale2sd(warp.pres.abs$dstn___)
grizzinc.ps <- scale2sd(warp.pres.abs$GrzzInE)
bhs.ps <- scale2sd(warp.pres.abs$BHSExtr)
biophys.ps <- scale2sd(warp.pres.abs$BphysEx)
pop.dens <- scale2sd(warp.pres.abs$Hmn_Dns)

bears_presence_ps <- warp.pres.abs$bears # Binomial bears
dom.farms.ps <- warp.pres.abs$Dm_Fr_T # Dominant farm type covariate -- non numeric
dom.farms.ps <- as.factor(warp.pres.abs$Dm_Fr_T) # Making this work as a factor

warp.pres.abs$CCSNAME <- as.factor(warp.pres.abs$CCSNAME)
warp.pres.abs$CCSUID <- as.factor(warp.pres.abs$CCSUID)

CCSUID.ps <- warp.pres.abs$CCSUID
CCSNAME.ps <- warp.pres.abs$CCSNAME

# Add an QUADRATIC term for Farm Count: 
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq.ps <- total.farms.ps*total.farms.ps

# Prep Data for Modeling in Rstanarm: ------------------------------------------------
# Make a mini data frame with just our predictors (no spatial info):
mini.warp.df.ps <-  data.frame(bears_presence_ps, b2pa.dist.ps, b2met.dist.ps, total.farms.ps, total.farms.ps, dom.farms.ps, grizzinc.ps, bhs.ps, biophys.ps, CCSUID.ps, CCSNAME.ps, pop.dens)

# Make our outcome to be factor type and create x and y variables:
mini.warp.df.ps$bears_presence_ps <- factor(mini.warp.df.ps$bears_presence_ps)
str(mini.warp.df.ps)

# Fitting our Posterior Regression: ---------------------------------------
# tutorial here: https://avehtari.github.io/modelselection/diabetes.html 

# Set our priors:
t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

# Build our posterior distribution: stan_glm returns the posterior dist for parameters describing the uncertainty related to unknown parameter values

# Start with our Null (Intercept-Only) Model:
post.int.only <- stan_glm(bears_presence_ps ~ 1 + (1 | CCSNAME.ps), 
                          data = mini.warp.df.ps,
                          family = binomial(link = "logit"), # define our binomial glm
                          prior = t_prior, prior_intercept = int_prior,
                          iter = 5000,
                          seed = SEED, refresh=0) # we add seed for reproducability

# Look at our simple model:
post.pa.simple <- stan_glm(bears_presence_ps ~ pop.dens + (1 | CCSNAME.ps), 
                           data = mini.warp.df.ps,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = int_prior, 
                           iter = 5000,
                           seed = SEED, refresh=0) # we add seed for reproducability

# Build out our Comparison Model (to compare to Global):
# For this one, we leave out the FarmCount quad term
post.pa.compare <- stan_glmer(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + pop.dens + (1 | CCSNAME.ps), 
                              data = mini.warp.df.ps,
                              family = binomial(link = "logit"), # define our binomial glm
                              prior = t_prior, prior_intercept = int_prior,
                              iter = 5000,
                              seed = SEED, refresh=0) # we add seed for reproducability
# If this has a bulk ESS error... need to run chains for longer

# Global Model: including all of our predictors (just not quadratic term)
post.pa.full <- stan_glmer(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.sq.ps + pop.dens + (1 | CCSNAME.ps), 
                           data = mini.warp.df.ps,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           seed = SEED, refresh=0) # we add seed for reproducability

summary(post.int.onlny)
summary(post.pa.simple)
summary(post.pa.compare)
summary(post.pa.full)

# Leave-one-out Cross_validation: -----------------------------------------
# Run a Leave-One-Out (LOO):
# Loo package implements fast Pareto smoothed leave-one-out-cross-val (PSIS-LOO) to compute expected log predictive density:
(loo.simplest <- loo(post.pa.simplest, save_psis = TRUE))
# Above we see that PSIS-LOO result is reliable as all Pareto k estimates are small (k< 0.5) Vehtari, Gelman and Gabry (2017a).
(loo.simple <- loo(post.pa.simple, save_psis = TRUE))

(loo.pa.full <- loo(post.pa.full, save_psis = TRUE))


plot(loo.pa.full, label_points = TRUE)
loo.pa.full # get the summary of our test

############## LOO Comparison to Baseline Model: -------------------------------------------

# Compute our baseline result without covariates:
post0.ps <- update(post.pa.simplest, formula = bears_presence_ps ~ 1, QR = FALSE, refresh=0)

# Compare to our baseline:
(loo.0 <- loo(post0.ps)) # computing the PSIS-LOO for our baseline model

loo_compare(loo.0, loo.simple, loo.simplest, loo.pa.full) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions

