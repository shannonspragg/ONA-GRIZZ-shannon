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
warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/Data/processed/warp.final.shp")
str(warp.df)

# Bring in our P(General Conflict) Raster for Extraction:
prob.gen.conf.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/prob_general_conf.tif")
names(prob.gen.conf.rast)[names(prob.gen.conf.rast) == "lyr.1"] <- "Probability of General Conflict"


# Import our Predictor Rasters: -------------------------------------------
  # Here we bring in all of our predictor rasters for the SOI region:

  # Distance to Nearest Protected Area (km):
dist2pa.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2pa_SOI_10km.tif" )

  # Dist to Grizzly Populations:
dist2grizzpop.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/dist2grizz_pop_raster.tif")

  # Animal & Meat Farming Density:
animal.farming.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/animal_production_density_raster.tif")

  # Ground Crop & Produce Production Density:
ground.crop.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/ground_crop_density_raster.tif")

  # Grizzinc:
grizzinc.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/grizz_inc_SOI_10km.tif")

  # Bear Density - Bear Habitat Suitability (BHS):
bhs.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/bhs_SOI_10km.tif")

  # Biophysical Current Map (Cumulative current flow shows the total current for each landscape pixel):
biophys.rast <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/biophys_SOI_10km.tif") # use this one

# CCS Region ID:
ccs.varint.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/CCS_varint_raster.tif" )
names(ccs.varint.rast)[names(ccs.varint.rast) == "CCSMean"] <- "CCS Varying Intercept Mean"


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

dist.2.pa.co <- scale2sd(warp.df$dst__PA)
animal.farm.dens.co <- warp.df$Anml_Fr
ground.crop.dens.co <- warp.df$Grnd_Cr
dist.2.grizzpop.co <- scale2sd(warp.df$dst__GP)
grizzinc.co <- scale2sd(warp.df$GrizzInc)
bhs.co <- scale2sd(warp.df$BHS)
biophys.co <- scale2sd(warp.df$Biophys)

bears_presence_co <- warp.df$bears # Binomial bears
prob.gen.conf <- warp.df$ProbGeneralConf  # This was already scaled

warp.df$CCSNAME <- as.factor(warp.df$CCSNAME)

CCSNAME.co <- warp.df$CCSNAME

which(is.na(warp.df$CCSNAME.co)) # none



# Fit Data for Rstanarm: --------------------------------------------------

# And do this for our conflict only df:
mini.warp.df.co <- data.frame(bears_presence_co, dist.2.pa.co, dist.2.grizzpop.co, animal.farm.dens.co, ground.crop.dens.co, 
                              grizzinc.co, bhs.co, biophys.co, CCSNAME.co, prob.gen.conf )

str(mini.warp.df.co)

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



# Semi-Model: Just including one ag variable 
post.co.semi <- stan_glmer(bears_presence_co ~ dist.2.pa.co + dist.2.grizzpop.co + animal.farm.dens.co + grizzinc.co + biophys.co + bhs.co + (1 | CCSNAME.co) + offset(prob.gen.conf), 
                             data = mini.warp.df.co,
                             family = binomial(link = "logit"), # define our binomial glm
                             prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                             iter = 5000, # Run for enough iterations to avoid errors
                             seed = SEED, refresh=0) # we add seed for reproducibility


post.co.offset <- stan_glmer(bears_presence_co ~ dist.2.pa.co + dist.2.grizzpop.co + animal.farm.dens.co + ground.crop.dens.co + grizzinc.co + biophys.co + bhs.co + (1 | CCSNAME.co) + offset(prob.gen.conf), 
                             data = mini.warp.df.co,
                             family = binomial(link = "logit"), # define our binomial glm
                             prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                             iter = 5000, # Run for enough iterations to avoid errors
                             seed = SEED, refresh=0) # we add seed for reproducibility

# Save an object to a file
saveRDS(post.co.offset, file = "/Users/shannonspragg/ONA_GRIZZ/Data/processed/post_co_offset.rds")
saveRDS(post.co.int, file = "/Users/shannonspragg/ONA_GRIZZ/Data/processed/post_co_int.rds")

# Restore the object
post.co.offset <- readRDS(file = "/Users/shannonspragg/ONA_GRIZZ/Data/processed/post_co_offset.rds")

##### Plot the posterior for just fixed effects of our different variables:
plot_model(post.co.full, sort.est = TRUE) # This plots just fixed effects
plot_model(post.co.offset)

# Posterior with varying intercepts:
co.full.plot<-plot(post.co.full, "areas", prob = 0.95, prob_outer = 1)
co.full.plot+ geom_vline(xintercept = 0)

co.offset.plot <- plot(post.co.offset, "areas", prob = 0.95, prob_outer = 1)
co.offset.plot+ geom_vline(xintercept = 0)

# Run Correlation Plot: ---------------------------------------------------
library(corrplot)

# Make a correlation matrix for our data dn predictors:
cor.matrix.df.co <- data.frame(dist.2.pa.co, dist.2.grizzpop.co, animal.farm.dens.co, ground.crop.dens.co, bhs.co, biophys.co, grizzinc.co)

cor.matrix.co <- cor(cor.matrix.df.co)
round(cor.matrix.co, 2)
# Simple correlation plots:
corrplot(cor.matrix.co, method = 'number', ) # colorful number
corrplot(cor.matrix.co, addCoef.col = 'black')

# Make a plot with proportional circles on a diagonal, coefficent numbers, and legend at the bottom:
predictor.cor.plot.co <- corrplot(cor.matrix.co, type = 'lower', order = 'hclust', tl.col = 'black',addCoef.col = 'grey',
                               cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))




# Plot our Area Under the Curve: ------------------------------------------
# Plot ROC for the Simple Posterior:
par(pty="s") # sets our graph to square
roc(bears_presence_co, post.co.offset$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 controls (bears 0) < 2062 cases (bears 1), Area under curve = 0.

# Add ROC curve for our semi model:
plot.roc(bears_presence_co, post.co.semi$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=60)

# Leave-one-out Cross_validation: -----------------------------------------
# Run a Leave-One-Out (LOO):
# Loo package implements fast Pareto smoothed leave-one-out-cross-val (PSIS-LOO) to compute expected log predictive density:

(loo.co.semi <- loo(post.co.semi, save_psis = TRUE))
(loo.co.offset <- loo(post.co.offset, save_psis = TRUE))

loo.comparison <- loo_compare(loo.co.semi, loo.co.offset) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions


plot(loo.co.full, label_points = TRUE)
loo.co.full # get the summary of our test

##### Comparison to Baseline Model: --

# Compute our baseline result without covariates:
post0.co <- update(post.co.int, formula = bears_presence_co ~ 1 + (1 | CCSNAME.ps), QR = FALSE, refresh=0)

# Compare to our baseline:
(loo.0.co <- loo(post.co.int)) # computing the PSIS-LOO for our baseline model

loo.comparison <- loo_compare(loo.0.co, loo.co.full, loo.co.offset) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions


# K-Fold Cross Validation: ------------------------------------------------
cv10folds <- kfold_split_stratified(K=10, x=mini.warp.df.ps$dom.farms.ps)

kfold.10.co <- kfold(post.co.offset, K=10, cores = getOption("mc.cores", 1))
kfold.10.co.int <- kfold(post.co.int, K=10, cores = getOption("mc.cores", 1))


# Save this as data:
saveRDS(kfold.10.co, file = "/Users/shannonspragg/ONA_GRIZZ/Data/processed/post_co_kfold10.rds")
saveRDS(kfold.10.co.int, file = "/Users/shannonspragg/ONA_GRIZZ/Data/processed/post_co_int_kfold10.rds")

# Plotting Mixed Effects: -------------------------------------------------

# Basic Mixed Effect Plot:
sjPlot::plot_model(post.co.offset)

# Labeled Effect Plot:
# Notes: axis labels should be in order from bottom to top. 
# To see the values of the effect size and p-value, set show.values and show.p= TRUE
post.co.offset.effects.plot <- sjPlot::plot_model(post.co.offset, 
                                         axis.labels=c("Bear Habitat Suitability", "Biophysical Connectivity Map of Bear Corridors", "Grizz Increase", "Ground Crop & Produce Farming Density",
                                                      "Animal & Meat Farming Density", " Distance to Extant Grizzly Populations", "Distance to Protected Area (km)"),
                                         show.values=TRUE, show.p=FALSE,
                                         title="Effects of Social & Environmental Variables on Bear Conflict")


# MIxed Effect Table:
sjPlot::tab_model(post.co.offset)


################## Make CCS Varying Intercept Raster:
# Bring in Data: ----------------------------------------------------------
soi.ccs.crop <- st_read( "/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_CCS_10km.shp")
# Bring in one of our rasters for rasterizing polygon data later:
soi.rast <- terra::rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/SOI_10km.tif") # SOI Region 10km

# Reproject the Data:
soi.ccs.reproj <- st_make_valid(soi.ccs.crop) %>% 
  st_transform(crs=crs(soi.rast))

# Check to see if they match:
st_crs(warp.df) == st_crs(soi.ccs.reproj) # [TRUE] 

# Extracting CCS Varying Intercept from Posterior: ------------------------

# Need to make the ranef(post.pa.full) into a dataframe and join by CCS name to our warp.pres.abs:
varying.int.means.co <- as.data.frame(ranef(post.co.offset))
vary.int.subset.co <- varying.int.means.co[ , c("grp", "condval")]

# Join the tab data with spatial:  
ccs.varint.join.co <- merge(soi.ccs.crop, vary.int.subset.co, by.x = "CCSNAME", by.y = "grp")

# Now that it's spatial, do a spatial join to assign a varying intercept mean to each point:
warp.varint.join.co <- st_join(warp.df, left = TRUE, ccs.varint.join.co) # join points

# Clean this up
warp.varint.join.co$PRNAME <- NULL
warp.varint.join.co$PRNTCDVSNC <- NULL
warp.varint.join.co$PRUID <- NULL
warp.varint.join.co$CDUID <- NULL
warp.varint.join.co$CPRVNCCD <- NULL
warp.varint.join.co$FTRCD <- NULL
warp.varint.join.co$CPRVNCCD <- NULL
warp.varint.join.co$PRNTCDVSNC <- NULL
warp.varint.join.co$FFCTVDT <- NULL
warp.varint.join.co$XPRDT <- NULL
warp.varint.join.co$OBJECTID <- NULL
warp.varint.join.co$AREA_SQM <- NULL
warp.varint.join.co$FEAT_LEN <- NULL 
warp.varint.join.co$CDNAME <- NULL
warp.varint.join.co$CDTYPE <- NULL
warp.varint.join.co$CPRVNCNM <- NULL
warp.varint.join.co$CCSNAME.y <- NULL
warp.varint.join.co$CCSUID.y <- NULL
warp.varint.join.co$CCSUID_ <- NULL

# Rename these quick:
names(warp.varint.join.co)[names(warp.varint.join.co) == "CCSNAME.x"] <- "CCSNAME"
names(warp.varint.join.co)[names(warp.varint.join.co) == "CCSUID.x"] <- "CCSUID"
names(warp.varint.join.co)[names(warp.varint.join.co) == "condval"] <- "CCS_Varying_Int"

# Make our CCS Raster: ----------------------------------------------------
# Make these spatvectors:
soi.ccs.sv <- vect(soi.ccs.reproj)
warp.co.sv <- vect(warp.varint.join.co)

# Make our CCS Post Mean Raster: ------------------------------------------
soi.ccs.rast <- terra::rasterize(soi.ccs.sv, soi.rast, field = "CCSNAME")

# Make Raster for our Posterior Means for CCS Varying Intercept: ---------------------
varint.means.rast.co <- terra::rasterize(warp.co.sv, soi.rast, field = "CCS_Varying_Int")
names(varint.means.rast.co)[names(varint.means.rast.co) == "OBJECTID"] <- "CCS Varying Intercept Mean Estimate"

# Extract Values of Posterior Means to CCS regions: 
warp.varint.mean.ext.co <- terra::extract(varint.means.rast.co, soi.ccs.sv, mean, na.rm = TRUE) 

# Create New Column(s) for Extracted Values:
soi.ccs.sv$CCSMean <- warp.varint.mean.ext.co[,2] 

# Make our CCS Post Mean Raster: ------------------------------------------
ccs.varint.means.rast.co <- terra::rasterize(soi.ccs.sv, soi.rast, field = "CCSMean")
names(ccs.varint.means.rast.co)[names(ccs.varint.means.rast.co) == "CCStMean"] <- "CCS Varying Intercept Means"

# Check this:
plot(ccs.varint.means.rast) 

# Save our CCS Post Means Raster: -----------------------------------------
terra::writeRaster(ccs.varint.means.rast.co, "/Users/shannonspragg/ONA_GRIZZ/Data/processed/CCS_varint_raster_co.tif" )
ccs.varint.means.rast.co <- rast("/Users/shannonspragg/ONA_GRIZZ/Data/processed/CCS_varint_raster_co.tif")


# Scale our Predictor Rasters: --------------------------------------------
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd.raster <-function(variable){(variable - global(variable, "mean", na.rm=TRUE)[,1])/(2*global(variable, "sd", na.rm=TRUE)[,1])}

# We can also do this Step by Step:

# Distance to PA:
d2pa.mean.co <- mean(warp.df$dst__PA)
d2pa.sub.mean.co <- dist2pa.rast - d2pa.mean.co
d2pa.sd.co <- sd(warp.df$dst__PA)
dist2pa.rast.co.sc <- d2pa.sub.mean.co / ( 2 * d2pa.sd.co)

# Distance to Grizzly Pops:
d2grizz.mean.co <- mean(warp.df$dst__GP)
d2grizz.sub.mean.co <- dist2grizzpop.rast - d2grizz.mean.co
d2grizz.sd.co <- sd(warp.df$dst__GP)
dist2grizzpop.rast.co.sc <- d2grizz.sub.mean.co / ( 2 * d2grizz.sd.co)

# Animal Farm Density:
animal.farm.mean.co <- mean(warp.df$Anml_Fr)
anim.f.sub.mean.co <- animal.farming.rast - animal.farm.mean.co
anim.f.sd.co <- sd(warp.df$Anml_Fr)
animal.farm.rast.co.sc <- anim.f.sub.mean.co / ( 2 * anim.f.sd.co)

# Ground Crop Density:
ground.crop.mean.co <- mean(warp.df$Grnd_Cr)
ground.c.sub.mean.co <- ground.crop.rast - ground.crop.mean.co
ground.c.sd.co <- sd(warp.df$Grnd_Cr)
ground.crop.rast.co.sc <- ground.c.sub.mean.co / ( 2 * ground.c.sd.co)

# Grizz Increase:
grizzinc.mean.co <- mean(warp.df$GrizzInc)
grizz.sub.mean.co <- grizzinc.rast - grizzinc.mean.co
grizzinc.sd.co <- sd(warp.df$GrizzInc)
grizzinc.rast.co.sc <- grizz.sub.mean.co / ( 2 * grizzinc.sd.co)

# Biophys:
biophys.mean.co <- mean(warp.df$Biophys)
bio.sub.mean.co <- biophys.rast - biophys.mean.co
biophys.sd.co <- sd(warp.df$Biophys)
biophys.rast.co.sc <- bio.sub.mean.co / ( 2 * biophys.sd.co)

# BHS:
bhs.mean.co <- mean(warp.df$BHS)
bhs.sub.mean.co <- bhs.rast - bhs.mean.co
bhs.sd.co <- sd(warp.df$BHS)
bhs.rast.co.sc <- bhs.sub.mean.co / ( 2 * bhs.sd.co)

# We don't need to scale the categorical rasters, CCS raster or the p(general conflict) raster


# Produce our P(Bear Conflict) Raster: ------------------------------------

# Make sure extents match:
ext(grizzinc.rast.co.sc) == ext(bhs.rast.co.sc) # TRUE
ext(biophys.rast.co.sc) == ext(animal.farm.rast.co.sc) #TRUE
ext(animal.farm.rast.co.sc) == ext(ground.crop.rast.co.sc) #TRUE

# View our Full Model Coefficients:
summary(post.co.offset)
fixef(post.co.offset)

# Stack these spatrasters:
bear.conf.rast.stack <- c(grizzinc.rast.co.sc, bhs.rast.co.sc, biophys.rast.co.sc, dist2pa.rast.co.sc, dist2grizzpop.rast.co.sc, animal.farm.rast.co.sc, ground.crop.rast.co.sc, ccs.varint.means.rast.co)
plot(bear.conf.rast.stack) # plot these all to check
# It looks like veg.melon raster has all 0 values, so isn't working

# Create P(all conflict) raster with our regression coefficients and rasters:
# Prob_conf_rast = Int.val + CCS + B_1est * PopDens…
#bear_conflict_rast <- -1.767194645 + ccs.varint.rast + (-0.272180709 * dist2pa.bear.rast.sc) + (0.190484001 * grizzinc.rast.sc) + (0.377704254 * biophys.rast.sc) + ( 0.001733723 * bhs.rast.sc) + (-0.205342727 * tot.farms.rast.co.sc) + (0.829898279  * tot.farms.sq.rast.co.sc) + 
#  ( 3.171972498 * prob.gen.conf.rast) + (0.936089290 * cattle.ranching.rast) + (1.226594906 * fruit.tree.nut.rast) + (1.186180400 * other.animal.rast) + (1.057287864 * other.crop.rast) + (0.391081848 * veg.melon.rast)

# Our full model with general conflict offset:
bear_conf_offset_rast <- -1.2012335 + ccs.varint.means.rast.co + (0.1940000 * dist2pa.rast.co.sc) + (0.8533170  * grizzinc.rast.co.sc) + (0.4597530 * biophys.rast.co.sc) + ( -0.2396923 * bhs.rast.co.sc) + (-0.4936497 * dist2grizzpop.rast.co.sc) + (-1.1418043 * animal.farm.rast.co.sc) + 
  ( 2.7521941 * ground.crop.rast.co.sc) 

# Convert the Raster to the Probability Scale:
p_BEAR_conf_rast <- app(bear_conflict_rast, fun=plogis)

p_BEAR_conf_offset_rast <- app(bear_conf_offset_rast, fun=plogis)

plot(p_BEAR_conf_rast)
plot(p_BEAR_conf_offset_rast) # Our p(bear conflict) with offset for general conflict

