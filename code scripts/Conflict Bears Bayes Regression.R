# WARP Conflict-only Bayes Regression --------------------------------------
    ### Here we do part two of our modeling - where we do a bayes regression with JUST our conflict points 
#   (1's = bears, 0's = all other species) and then add in our p(general conflict) from the pseudo-abs regression as a predictor

# Load Packages: ----------------------------------------------------------
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




# Import Data: ------------------------------------------------------------

# Our just-conflict point data:
warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10_ccs.shp")
str(warp.df)

# Bring in our P(General Conflict) Raster for Extraction:
prob.gen.conf.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/prob_general_conf.tif")


# Import our Predictor Rasters: -------------------------------------------
# Here we bring in all of our predictor rasters for the SOI region:

# Dominant Farm Type by CCS Region:
dom.farms.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/dom_farm_type_raster.tif")

# Total Farm Count by CCS Region:
tot.farms.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/total_farm_count_raster.tif" )

# Total Farm Count Squared:
tot.farms.sq.rast <- tot.farms.rast * tot.farms.rast # I am not sure how to do this one...

# Dist to PA's , buffered for bears:
dist2pa.bear.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/10k_hadist2pa_raster.tif")

# Grizzinc:
grizzinc.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/grizz_inc_SOI_10km.tif")

# Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/bhs_SOI_10km.tif")

# Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Predictor Rasters/biophys_SOI_10km.tif") # use this one

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
names(prob.gen.conf.rast)[names(prob.gen.conf.rast) == "lyr.1"] <- "Probability of General Conflict"
names(grizzinc.rast)[names(grizzinc.rast) == "griz_inc"] <- "Support for Grizzly Increase"
names(biophys.rast)[names(biophys.rast) == "cum_currmap"] <- "Biophysical Current Map"



# Extract P(General Conflict) to WARP Points: -----------------------------

# Check Projections:
plot(prob.gen.conf.rast)
plot(warp.df, add=TRUE) # Yup, looks good!

# Here we buffer the WARP points by 5km before extracting the attributes from the current maps
warp.all.buf <- warp.df %>% 
  st_buffer(., 5000)
plot(st_geometry(warp.all.buf)) # Check the buffers

# Need to make points a SpatVector:
warp.sv <- vect(warp.all.buf)

# Here I will extract the mean values from each raster to the buffered points
warp.prob.gen.ext <- terra::extract(prob.gen.conf.rast, warp.sv, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
warp.df$ProbGeneralConf <- warp.prob.gen.ext[,2]

# Check for NA's:
which(is.na(warp.df$ProbGeneralConf)) #none



# Scale the Variables: ----------------------------------------------------------
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

b2pa.dist.co.sc <- scale2sd(warp.df$ds__PA_)
total.farms.co.sc <- scale2sd(warp.df$Ttl_F_C)
b2met.dist.co.sc <- scale2sd(warp.df$dstn___)
grizzinc.co.sc <- scale2sd(warp.df$GrzzInE)
bhs.co.sc <- scale2sd(warp.df$BHSExtr)
biophys.co.sc <- scale2sd(warp.df$BphysEx)
total.farms.warp.co <- warp.df$Ttl_F_C
total.farms.sq.co <- total.farms.warp * total.farms.warp

bears_presence_co <- warp.df$bears # Binomial bears
dom.farms.co <- warp.df$Dm_Fr_T # Dominant farm type covariate -- non numeric
dom.farms.co <- as.factor(warp.df$Dm_Fr_T) # Making this work as a factor
prob.gen.conf <- warp.df$ProbGeneralConf  # This was already scaled

warp.df$CCSNAME <- as.factor(warp.df$CCSNAME)
warp.df$CCSUID <- as.factor(warp.df$CCSUID)

CCSUID.co <- warp.df$CCSUID
CCSNAME.co <- warp.df$CCSNAME

which(is.na(warp.df$CCSNAME.ps)) # none
which(is.na(warp.df$CCSUID.ps)) # none

# Add an QUADRATIC term for Farm Count: 
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq.co.sc <- total.farms.co.sc*total.farms.co.sc


# Fit Data for Rstanarm: --------------------------------------------------

# And do this for our conflict only df:
mini.warp.df.co <- data.frame(bears_presence_co, b2pa.dist.co.sc, total.farms.co.sc, total.farms.sq.co.sc, dom.farms.co, 
                              grizzinc.co.sc, bhs.co.sc, biophys.co.sc, CCSUID.co, CCSNAME.co, prob.gen.conf )

# Make sure this is a factor:
mini.warp.df.co$bears_presence_co <- factor(mini.warp.df.co$bears_presence_co)

# Fit the Regression: -----------------------------------------------------

t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

# Fit our full model with conlfict-only df:

# Intercept-only model:
post.co.int <- stan_glmer(bears_presence_co ~ 1 + (1 | CCSNAME.co), 
                           data = mini.warp.df.co,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = int_prior, QR=FALSE,
                           iter = 5000,
                           seed = SEED, refresh=0) # we add seed for reproducibility

# Full model:
post.co.full <- stan_glmer(bears_presence_co ~ b2pa.dist.co.sc + dom.farms.co + total.farms.co.sc + total.farms.sq.co.sc + grizzinc.co.sc + biophys.co.sc + bhs.co.sc + (1 | CCSNAME.co) + prob.gen.conf, 
                           data = mini.warp.df.co,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                           iter = 5000,
                           seed = SEED, refresh=0) # we add seed for reproducibility
summary(post.co.full)



# Plot our Area Under the Curve: ------------------------------------------
# Plot ROC for the Simple Posterior:
par(pty="s") # sets our graph to square
roc(bears_presence_co, post.co.full$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 controls (bears 0) < 2062 cases (bears 1), Area under curve = 0.

# Leave-one-out Cross_validation: -----------------------------------------
# Run a Leave-One-Out (LOO):
# Loo package implements fast Pareto smoothed leave-one-out-cross-val (PSIS-LOO) to compute expected log predictive density:

(loo.co.full <- loo(post.co.full, save_psis = TRUE))


plot(loo.co.full, label_points = TRUE)
loo.co.full # get the summary of our test

##### Comparison to Baseline Model: --

# Compute our baseline result without covariates:
post0.co <- update(post.co.int, formula = bears_presence_co ~ 1 + (1 | CCSNAME.ps), QR = FALSE, refresh=0)

# Compare to our baseline:
(loo.0.co <- loo(post0.ps)) # computing the PSIS-LOO for our baseline model

loo.comparison <- loo_compare(loo.0.co, loo.co.full) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions





# Plotting Mixed Effects: -------------------------------------------------

# Basic Mixed Effect Plot:
sjPlot::plot_model(post.co.full)

# MIxed Effect Table:
sjPlot::tab_model(post.co.full)


# Scale our Predictor Rasters: --------------------------------------------
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd.raster <-function(variable){(variable - global(variable, "mean", na.rm=TRUE)[,1])/(2*global(variable, "sd", na.rm=TRUE)[,1])}

# We can also do this Step by Step:

# Distance to PA:
d2pa.mean.co <- mean(warp.df$ds__PA_)
d2pa.sub.mean.co <- dist2pa.bear.rast - d2pa.mean.co
d2pa.sd.co <- sd(warp.df$ds__PA_)
dist2pa.bear.rast.sc <- d2pa.sub.mean.co / ( 2 * d2pa.sd.co)

# Total Farm Count:
ttl.farm.mean.co <- mean(warp.df$Ttl_F_C)
ttl.f.sub.mean.co <- tot.farms.rast - ttl.farm.mean.co
ttl.f.sd.co <- sd(warp.df$Ttl_F_C)
tot.farms.rast.co.sc <- ttl.f.sub.mean.co / ( 2 * ttl.f.sd.co)

# Total Farms Sq:
ttl.farm.sq.mean.co <- mean(total.farms.sq.co)
ttl.f.sq.sub.mean.co <- tot.farms.sq.rast - ttl.farm.sq.mean.co
ttl.f.sq.sd.co <- sd(total.farms.sq.co)
tot.farms.sq.rast.co.sc <- ttl.f.sq.sub.mean.co / ( 2 * ttl.f.sq.sd.co)

# Grizz Increase:
grizzinc.mean <- mean(warp.df$GrzzInE)
grizz.sub.mean <- grizzinc.rast - grizzinc.mean
grizzinc.sd <- sd(warp.df$GrzzInE)
grizzinc.rast.sc <- grizz.sub.mean / ( 2 * grizzinc.sd)

# Biophys:
biophys.mean <- mean(warp.df$BphysEx)
bio.sub.mean <- biophys.rast - biophys.mean
biophys.sd <- sd(warp.df$BphysEx)
biophys.rast.sc <- bio.sub.mean / ( 2 * biophys.sd)

# BHS:
bhs.mean <- mean(warp.df$BHSExtr)
bhs.sub.mean <- bhs.rast - bhs.mean
bhs.sd <- sd(warp.df$BHSExtr)
bhs.rast.sc <- bhs.sub.mean / ( 2 * bhs.sd)

# We don't need to scale the categorical rasters, CCS raster or the p(general conflict) raster


# Produce our P(Bear Conflict) Raster: ------------------------------------

# Make sure extents match:
ext(grizzinc.rast.sc) == ext(bhs.rast.sc) # TRUE
ext(biophys.rast.sc) == ext(tot.farms.rast.sc) #TRUE
ext(tot.farms.sq.rast.sc) == ext(cattle.ranching.rast) #TRUE

# View our Full Model Coefficients:
summary(post.co.full)
fixef(post.co.full)

# Stack these spatrasters:
bear.conf.rast.stack <- c(grizzinc.rast.sc, bhs.rast.sc, biophys.rast.sc, dist2pa.bear.rast.sc, tot.farms.rast.co.sc, tot.farms.sq.rast.co.sc, cattle.ranching.rast, ccs.varint.rast, fruit.tree.nut.rast, other.animal.rast, other.crop.rast, veg.melon.rast, prob.gen.conf.rast)
plot(bear.conf.rast.stack) # plot these all to check
# It looks like veg.melon raster has all 0 values, so isn't working

# Create P(all conflict) raster with our regression coefficients and rasters:
# Prob_conf_rast = Int.val + CCS + B_1est * PopDens…
bear_conflict_rast <- -1.767194645 + ccs.varint.rast + (-0.272180709 * dist2pa.bear.rast.sc) + (0.190484001 * grizzinc.rast.sc) + (0.377704254 * biophys.rast.sc) + ( 0.001733723 * bhs.rast.sc) + (-0.205342727 * tot.farms.rast.co.sc) + (0.829898279  * tot.farms.sq.rast.co.sc) + 
  ( 3.171972498 * prob.gen.conf.rast) + (0.936089290 * cattle.ranching.rast) + (1.226594906 * fruit.tree.nut.rast) + (1.186180400 * other.animal.rast) + (1.057287864 * other.crop.rast) + (0.391081848 * veg.melon.rast)

# Convert the Raster to the Probability Scale:
p_BEAR_conf_rast <- app(bear_conflict_rast, fun=plogis)

plot(p_BEAR_conf_rast)


