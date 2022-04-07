# Bayes Regression: Pseudo - Abs Model: -----------------------------------
    ## Here we run a simple Bayes regression using our WARP pseudo-absence points instead of the species vs bears


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
library(randomForest)


# Import Data: ------------------------------------------------------------

warp.pres.abs <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_presabs_complete.shp")
str(warp.pres.abs)

# Scale the Variables: ----------------------------------------------------------

# We can use the scale function to center our predictors by subtracting the means (center= TRUE) and scaling by dividing by their standard deviation (scale=TRUE)
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

b2pa.dist.ps <- scale2sd(warp.pres.abs$ds__PA_)
total.farms.ps <- scale2sd(warp.pres.abs$Ttl_F_C)
b2met.dist.ps <- scale2sd(warp.pres.abs$dstn___)
grizzinc.ps <- scale2sd(warp.pres.abs$GrzzInE)
bhs.ps <- scale2sd(warp.pres.abs$BHSExtr)
biophys.ps <- scale2sd(warp.pres.abs$BphysEx)
human.dens <- scale2sd(warp.pres.abs$Human_Dens)

bears_presence_ps <- warp.pres.abs$bears # Binomial bears
dom.farms.ps <- warp.pres.abs$Dm_Fr_T # Dominant farm type covariate -- non numeric
dom.farms.ps <- as.factor(warp.pres.abs$Dm_Fr_T) # Making this work as a factor

warp.pres.abs$CCSNAME <- as.factor(warp.pres.abs$CCSNAME)
warp.pres.abs$CCSUID <- as.factor(warp.pres.abs$CCSUID)

CCSUID.ps <- warp.pres.abs$CCSUID
CCSNAME.ps <- warp.pres.abs$CCSNAME

which(is.na(warp.pres.abs$CCSNAME.ps)) # Like 200 NA's!!
which(is.na(warp.pres.abs$CCSUID.ps)) # Like 200 NA's!!

# Add an QUADRATIC term for Farm Count: -----------------------------------
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq.ps <- total.farms.ps*total.farms.ps


# Fit Model with Rstanarm: ------------------------------------------------

# Take a look at our data:
summary(warp.pres.abs)
mini.warp.df.ps <-  data.frame(bears_presence_ps, b2pa.dist.ps, b2met.dist.ps, total.farms.ps, total.farms.ps, dom.farms.ps, grizzinc.ps, bhs.ps, biophys.ps, CCSUID.ps, CCSNAME.ps, human.dens)
str(mini.warp.df.ps)

############# Make our outcome to be factor type and create x and y variables:
mini.warp.df.ps$bears_presence_ps <- factor(mini.warp.df.ps$bears_presence_ps)
str(mini.warp.df.ps)

# preparing the inputs
x <- model.matrix(bears_presence_ps ~ . - 1, data = mini.warp.df.ps)
y <- mini.warp.df.ps$bears_presence_ps 
y <-formula(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.ps + grizzinc.ps + bhs.ps + biophys.ps)


n=dim(mini.warp.df.ps)[1]
p=dim(mini.warp.df.ps)[2]


# Fitting our Posterior Regression: ---------------------------------------
# tutorial here: https://avehtari.github.io/modelselection/diabetes.html 

# Set our initial priors to students t with df of 7, and a scale of 2.5 (reasonable default fir prior when coefficients should be close to zero but have some chance of being large:
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# Build our posterior distribution: stan_glm returns the posterior dist for parameters describing the uncertainty related to unknown parameter values
post1.ps <- stan_glm(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.ps + grizzinc.ps + bhs.ps + biophys.ps, 
                  data = mini.warp.df.ps,
                  family = binomial(link = "logit"), # define our binomial glm
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = SEED, refresh=0) # we add seed for reproducability

# Add in the human density variable:
post2.ps <- stan_glm(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.ps + grizzinc.ps + bhs.ps + biophys.ps + human.dens, 
                     data = mini.warp.df.ps,
                     family = binomial(link = "logit"), # define our binomial glm
                     prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                     seed = SEED, refresh=0) # we add seed for reproducability

##### Add in a Varying Intercept for SOI CCS Region:
post2ps.var.int <- stan_glmer(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc + (1 | CCSNAME), 
                            data = mini.warp.df.ps,
                            family = binomial(link = "logit"), # define our binomial glm
                            prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                            seed = SEED, refresh=0) # we add seed for reproducability



# Leave-one-out Cross_validation: -----------------------------------------

############## Run a Leave-One-Out (LOO):
# Loo package implements fast Pareto smoothed leave-one-out-cross-val (PSIS-LOO) to compute expected log predictive density:
(loo1.ps <- loo(post1.ps, save_psis = TRUE))
# Above we see that PSIS-LOO result is reliable as all Pareto k estimates are small (k< 0.5) Vehtari, Gelman and Gabry (2017a).

plot(loo1.ps, label_points = TRUE)
loo1 # get the summary of our test

############## Comparison to Baseline Model: -------------------------------------------

# Compute our baseline result without covariates:
post0.ps <- update(post1.ps, formula = bears_presence_ps ~ 1, QR = FALSE, refresh=0)

# Compare to our baseline:
(loo0.ps <- loo(post0.ps)) # computing the PSIS-LOO for our baseline model

loo_compare(loo0.ps, loo1.ps) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions


#  Find area under the curve & Compare Posterior Predictions: ---------------------------------------------

area_under_curve(med.preds.int, linpred.int, method = "trapezoid")

# Running example -- obese = bears_pres

# Model for the posterior1
stan.glm.fit1.ps <- stan_glm(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.ps + grizzinc.ps + bhs.ps + biophys.ps, 
                          data = mini.warp.df.ps,
                          family = binomial(link = "logit"), # define our binomial glm
                          prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                          seed = SEED, refresh=0) # we add seed for reproducability

stan.glm.fit2.ps <- stan_glm(bears_presence_ps ~ b2pa.dist.ps + b2met.dist.ps + dom.farms.ps + total.farms.ps + total.farms.ps + grizzinc.ps + bhs.ps + biophys.ps + human.dens, 
                             data = mini.warp.df.ps,
                             family = binomial(link = "logit"), # define our binomial glm
                             prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                             seed = SEED, refresh=0) # we add seed for reproducability

# Plot ROC:
par(pty="s") # sets our graph to square
roc(bears_presence_ps, stan.glm.fit1.ps$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 conrols (bears 0) < 2062 cases (bears 1), Area under curve = 0.6547

# Plot ROC of Pseudo-abs reg + human density variable: this doesn't look any different
roc(bears_presence_ps, stan.glm.fit2.ps$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 conrols (bears 0) < 2062 cases (bears 1), Area under curve = 0.6547


# Add ROC curve for our post1 (without pseudo-absences):
plot.roc(bears_presence, post.var.int.fit$fitted.values, percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Pseudo-Abs Regression", "Varying Int. Regression"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)
#******* What this tells us: the pseudo-absence model has a 94.9% discrimination (model predictions are correct 94.9% of the time) which is 
# much better compared to our model using the 'other species' as 0's. 





