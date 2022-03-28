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


