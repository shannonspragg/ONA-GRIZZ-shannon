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

b2pa.dist.co <- scale2sd(warp.df$ds__PA_)
total.farms.co <- scale2sd(warp.df$Ttl_F_C)
b2met.dist.co <- scale2sd(warp.df$dstn___)
grizzinc.co <- scale2sd(warp.df$GrzzInE)
bhs.co <- scale2sd(warp.df$BHSExtr)
biophys.co <- scale2sd(warp.df$BphysEx)

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
total.farms.sq.co <- total.farms.co*total.farms.co


# Fit Data for Rstanarm: --------------------------------------------------

# And do this for our conflict only df:
mini.warp.df.co <- data.frame(bears_presence_co, b2pa.dist.co, b2met.dist.co, total.farms.co, total.farms.co, total.farms.sq.co, dom.farms.co, grizzinc.co, bhs.co, biophys.co, CCSUID.co, CCSNAME.co )

# Make sure this is a factor:
mini.warp.df.co$bears_presence_co <- factor(mini.warp.df.co$bears_presence_co)

# Fit the Regression: -----------------------------------------------------

t_prior <- student_t(df = 7, location = 0, scale = 1.5)
int_prior <- normal(location = 0, scale = NULL, autoscale = FALSE)

# Fit our full model with conlfict-only df:
post.co.full <- stan_glmer(bears_presence_co ~ dom.farms.co + total.farms.co + total.farms.sq.co + grizzinc.co + biophys.co + bhs.co + (1 | CCSNAME.co) + prob.gen.conf, 
                           data = mini.warp.df.co,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = int_prior, QR=TRUE,
                           iter = 5000,
                           seed = SEED, refresh=0) # we add seed for reproducibility
summary(post.co.full)


# Scale our Predictor Rasters: --------------------------------------------
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd.raster <-function(variable){(variable - global(variable, "mean", na.rm=TRUE)[,1])/(2*global(variable, "sd", na.rm=TRUE)[,1])}

tot.farms.rast.sc <- scale2sd.raster(tot.farms.rast)
tot.farms.sq.rast.sc <- scale2sd.raster(tot.farms.sq.rast)
grizzinc.rast.sc <- scale2sd.raster(grizzinc.rast)
bhs.rast.sc <- scale2sd.raster(bhs.rast)
biophys.rast.sc <- scale2sd.raster(biophys.rast)

# We don't need to scale the categorical rasters, CCS raster or the p(general conflict) raster


# Produce our P(Bear Conflict) Raster: ------------------------------------

# Make sure extents match:
ext(grizzinc.rast.sc) == ext(bhs.rast.sc) # TRUE
ext(biophys.rast.sc) == ext(tot.farms.rast.sc) #TRUE
ext(tot.farms.sq.rast.sc) == ext(cattle.ranching.rast) #TRUE

# View our Full Model Coefficients:
summary(post.co.full)


# Stack these spatrasters:
bear.conf.rast.stack <- c(grizzinc.rast.sc, bhs.rast.sc, biophys.rast.sc, tot.farms.rast.sc, tot.farms.sq.rast.sc, cattle.ranching.rast.sc, ccs.varint.rast, fruit.tree.nut.rast.sc, other.animal.rast.sc, other.crop.rast.sc, veg.melon.rast.sc, prob.gen.conf.rast)
plot(conf.rast.stack) # plot these all to check
# It looks like veg.melon raster has all 0 values, so isn't working

# Create P(all conflict) raster with our regression coefficients and rasters:
# Prob_conf_rast = Int.val + CCS + B_1est * PopDens…
conflict_rast <- -3.2624640 + ccs.varint.rast + (-1.3887130 * dist2pa.rast.sc) + (-1.2986664 * dist2metro.rast.sc) + (-0.2535196 * tot.farms.rast.sc) + (0.4583006 * tot.farms.sq.rast.sc) + (0.7262155 * hm.dens.rast.sc) + (-0.2987515 * cattle.ranching.rast.sc) + (1.1384551 * fruit.tree.nut.rast.sc) + (1.5483847 * other.animal.rast.sc) + (1.6437851 * other.crop.rast.sc) + (0.2613143 * veg.melon.rast)

# Convert the Raster to the Probability Scale:
p_conf_rast <- app(conflict_rast, fun=plogis)

plot(p_conf_rast)


