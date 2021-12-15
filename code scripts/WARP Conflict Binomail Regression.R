# WARP Conflict Binomial Linear Regression ------------------------------------------------
# This script will be where I import the all species master df and configure it into the binomial 
# linear regression analysis code
# Important Questions -- NA


# Load Packages -----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
# Import the All Species Master df ----------------------------------------

warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Master DF (+CS resistance values)/WARP Master DF (+CS resist values).shp")

# Create Binomial GLM -- Bring in All Covariates -----------------------------------------------------
bears_presence <- warp.all.sp$bears # Binomial bears
b2pa.distance <- scale(warp.all.sp$ds__PA_) # Dist to PA covariate
dom.farms <- warp.all.sp$Dm_Fr_T # Dominant farm type covariate -- non numeric
dom.farms <- as.factor(warp.all.sp$Dm_Fr_T) # Making this work as a factor
total.farms <- warp.all.sp$Ttl_F_C # Total farm count covariate
b2met.dist <- warp.all.sp$dstn___ # Dist to metro covariate
grizzinc.cs.social <- warp.all.sp$GrzzInE
bear.habitat.bhs <- warp.all.sp$BHSExtr
social <- warp.all.sp$SrvyRsE
biophys.cs <- warp.all.sp$BphysEx

# Check for NA's ----------------------------------------------------------
which(is.na(dom.farms)) # No more NA's
which(is.na(total.farms)) # Ditto
which(is.na(bear.habitat.bhs)) # NA's all gone
which(is.na(biophys.cs)) # No NA's


# Scaling Individual Predictors -------------------------------------------
# We can use the scale function to center our predictors by subtracting the means (center= TRUE) and scaling by dividing by their standard deviation (scale=TRUE)
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

b2pa.dist.sc <- scale2sd(b2pa.distance)
dom.farm.sc <- scale2sd(dom.farms) # Need to find how to scale categorical var
total.farms.sc <- scale2sd(total.farms)
b2met.dist.sc <- scale2sd(b2met.dist)
grizzinc.cs.sc <- scale2sd(grizzinc.cs.social)
bhs.sc <- scale2sd(bear.habitat.bhs)
social.sc <- scale2sd(social)
biophys.sc <- scale2sd(biophys.cs)

# Below is scale by variable - mean / 1 SD:
#b2pa.dist.sc <- scale(b2pa.distance, center = TRUE, scale = TRUE)
#dom.farms.sc <- scale(dom.farms) # won't scale bc is not numeric
#total.farms.sc <- scale(total.farms, center = TRUE, scale = TRUE)
#b2met.dist.sc <- scale(b2met.dist, center = TRUE, scale = TRUE)
#grizzinc.cs.sc <- scale(grizzinc.cs.social, center = TRUE, scale = TRUE)
#bhs.sc <- scale(bear.habitat.bhs, center = TRUE, scale = TRUE)
#social.sc <- scale(social, center = TRUE, scale = TRUE)
#biophys.sc <- scale(biophys.cs, center = TRUE, scale = TRUE)


# Run models with scaled predictors:
# Run Adam's model sets:
fullmod.sc <- glm(bears_presence ~ bhs.sc + grizzinc.cs.sc + social.sc + biophys.sc, family = "binomial")
ecol.mod.sc <- glm(bears_presence ~ bhs.sc + biophys.sc, family = "binomial") 
social.mod.sc <- glm(bears_presence ~ social.sc + grizzinc.cs.sc, family = "binomial") 
intercept.only.sc <- glm(bears_presence~1, family=binomial(link=logit))

# Add in Covs to Full Mod:
fullmod.covs.sc <- glm(bears_presence ~ bhs.sc + grizzinc.cs.sc + social.sc + biophys.sc + b2pa.dist.sc + b2met.dist.sc
                        + total.farms.sc + dom.farms, family = "binomial")

# Compare these to the unscaled glm's below:
summary(fullmod.covs.glm)
summary(fullmod.covs.sc)# see big difference in coefficent values
summary(fullmod.sc) 
summary(ecol.mod.sc)
summary(social.mod.sc)
summary(intercept.only.sc)

# Individual covariate models (SCALED):
bhs.glm.sc <- glm(bears_presence ~ bhs.sc, family = "binomial")
grizz.inc.cs.glm.sc <- glm(bears_presence ~ grizzinc.cs.sc, family = "binomial")
social.glm.sc <- glm(bears_presence ~ social.sc, family = "binomial")
biophys.glm.sc <- glm(bears_presence ~ biophys.sc, family = "binomial")

b2pa.glm.sc <- glm(bears_presence ~ b2pa.dist.sc, family = "binomial")
b2met.glm.sc <- glm(bears_presence ~ b2met.dist.sc, family = "binomial")
tot.farm.glm.sc <- glm(bears_presence ~ total.farms.sc, family = "binomial")
dom.farm.glm.sc <- glm(bears_presence ~ dom.farms, family = "binomial")


# Prep Simulation to Match Data with Binomial Reg: ------------------------
# Creating a simulation to see if it returns close to the intercept and slope we input initially 
# https://daviddalpiaz.github.io/appliedstats/logistic-regression.html

# First we create a slope and intercept for our bears and covariates:
Intercept=0.3 # This is set for a 0-1 value, representing bear presence
Slope=2.1 # need a slope for each variable, so we create a matrix for each variable 
# Do this like this: slope (B) <- runif(15,-2,2) https://data.library.virginia.edu/simulating-a-logistic-regression-model/
B_slope <- runif(4, -2,2) # Created a randomized slope matrix for the variables
# Or, just make a slope for each variable and test with that:
Slope_distPA=1.6 
Slope_distmet=-.4 
Slope_totalfarms=-.6 

# Next we run p using our slopes and covariates:
p=plogis(Intercept+Slope*b2pa.distance) # simulating p with our intercept and slope, to fit model to p

# p=plogis(Intercept+B_slope[[1]]*b2pa.distance + B_slope[[2]]*b2met.dist + B_slope[[4]]*total.farms) 
# Leaving out B_slope[[3]]*dom.farms for now bc it isn't numeric
p=plogis(Intercept+Slope_distPA*b2pa.distance + Slope_distmet*b2met.dist + Slope_totalfarms*total.farms) # simulating p with our intercept and slope, to fit model to p

# Creating a simulation with probability p:
y_sim <- rbinom(length(bears_presence), 1, prob = p) # running the simulation with probability p

# Now let's run this with the simulation to get a simulated model:
sim.glm <- glm(y_sim ~ b2pa.distance + b2met.dist + total.farms, family = "binomial") # running the regression based on the simulation

summary(sim.glm) # This shows that dist to metro area and total farm count are "significant"
coef(sim.glm) # Gives us the Intercept and slopes of the variables
confint(sim.glm, level = 0.95)



# Running Linear Regressions ----------------------------------------------
# Here I will be running the three models Adam requested:
# 1. Full model: glm(bear_pres ~ BHS (grizz dens) + CS Social + Social (survey resist) + Biophys CS)
# 2. Ecol mod: glm(bear_pres ~ BHS (grizz dens) + CS Biophys)
# 3. Soc mod: glm(bear_pres ~ Social + CS Social (grizz inc))

# Matt's model is the "full" model plus all added predictors:
# Full model + covs: glm(bears_presence ~ bear.habitat.bhs + grizzinc.cs.social + social + biophys.cs + b2pa.distance + b2met.dist
# + total.farms + dom.farms

# Variables Described Below:
# BHS - CS of bear habitat suitability based on grizzly density estimate (from Clayton)
# Grizzinc CS Social - Values from the CS of Social resistance (survey responses supporting Grizz Increase)
# Social - resistance values for survey responses supporting Grizz Increase (input for Grizzinc CS)
# CS Biophys - Values from the CS of Biophysical only raster (Human Influence Index + topographic roughness)
# b2pa.distance - Minimum distance of conflict report points to nearest protected area (PA)
# b2met.dist - minimum distance of conflict report points to nearest metropolitan area 
# total.farms - the total count of farms by CCS region, assigned to each conflict point
# dom.farms - the dominant farm type (most popular type of agrictulture) by CCS region, assigned to each point


# Run model sets:
fullmod.glm <- glm(bears_presence ~ bear.habitat.bhs + grizzinc.cs.social + social + biophys.cs, family = "binomial")
ecol.mod.glm <- glm(bears_presence ~ bear.habitat.bhs + biophys.cs, family = "binomial") 
social.mod.glm <- glm(bears_presence ~ social + grizzinc.cs.social, family = "binomial") 
intercept.only.glm <- glm(bears_presence~1, family=binomial(link=logit))

# Add in Covs to Full Mod:
fullmod.covs.glm <- glm(bears_presence ~ bear.habitat.bhs + grizzinc.cs.social + social + biophys.cs + b2pa.distance + b2met.dist
                        + total.farms + dom.farms, family = "binomial")


# Individual covariate models:
bhs.glm <- glm(bears_presence ~ bear.habitat.bhs, family = "binomial")
grizz.inc.cs.glm <- glm(bears_presence ~ grizzinc.cs.social, family = "binomial")
social.glm <- glm(bears_presence ~ social, family = "binomial")
biophys.glm <- glm(bears_presence ~ biophys.cs, family = "binomial")

b2pa.glm <- glm(bears_presence ~ b2pa.distance, family = "binomial")
b2met.glm <- glm(bears_presence ~ b2met.dist, family = "binomial")
tot.farm.glm <- glm(bears_presence ~ total.farms, family = "binomial")
dom.farm.glm <- glm(bears_presence ~ dom.farms, family = "binomial")


# Check Summaries ---------------------------------------------------------
summary(fullmod.glm) # AIC 43308
summary(fullmod.glm)$coefficients # Pull up summary for coefficients
summary(ecol.mod.glm) # AIC 43672
summary(social.mod.glm) # AIC 43412

summary(fullmod.covs.glm) # AIC 41177
# INTERESTING: lowest AIC is in the full model + covs, pretty significantly

# Summaries for individual models
summary(bhs.glm) # p of 0.0264 *, AIC 44108
summary(grizz.inc.cs.glm) # p of <2e-16 *** , AIC 44114
summary(social.glm) # p of <2e-16 *** , AIC 43994
summary(biophys.glm) # p of <2e-16 *** , AIC 43778

summary(b2pa.glm) # p of 2.25e-14 *** , AIC 44143
summary(b2met.glm) # p of <2e-16 *** , AIC 44000
summary(tot.farm.glm) # p of <2e-16 ***, AIC 43161
summary(dom.farm.glm) # p of .000579 *** (veg & melon), .001741 ** (cattle), .038583 * (beef, feedlots), AIC 43225



# Running AIC for Model Comparison ----------------------------------------
# Run AIC to Compare Raw (unscaled) Models:
all.mod.aic <- AIC(fullmod.glm, fullmod.covs.glm, ecol.mod.glm, social.mod.glm, bhs.glm, grizz.inc.cs.glm, social.glm, biophys.glm, b2pa.glm, b2met.glm, tot.farm.glm, dom.farm.glm, intercept.only.glm)

# Running AIC on Scaled Models:
all.mod.scaled.aic <- AIC(fullmod.sc, fullmod.covs.sc, ecol.mod.sc, social.mod.sc, bhs.glm.sc, grizz.inc.cs.glm.sc, social.glm.sc, biophys.glm.sc, b2pa.glm.sc, b2met.glm.sc, tot.farm.glm.sc, dom.farm.glm.sc, intercept.only.glm)


# Model Selection with ANOVA ----------------------------------------------
anova(fullmod.covs.glm) # major deviance with tot/dom farms, grizzinc.cs , & social
anova(fullmod.sc, fullmod.covs.sc) # largest deviance with grizzinc.cs & social

# The above deviances indicate that we should scale our predictors and then re-run the models

# Adding an interaction to the model:
glm_mod_interaction = glm(bears_presence ~ b2pa.distance + b2met.dist + total.farms + b2pa.distance:b2met.dist + total.farms:dom.farms, 
                          family = "binomial")
summary(glm_mod_interaction) 
# Appears to be a likely interaction between total farms and the dominant farm types

# For glm, one way to simplify what predictors to take out, is that if the variance is zero, it has such a small effect, and can be removed to better fit the model
# In bayesian, you can fit the full model with strong priors and having regularization for those models (lets you fit small variances)

# Bayesian takes more time, but you can fit better models with strong priors and can eliminate uncertainty by producing distributions for all
# Frequentest has some model evaluation tools that can make these things a bit more straightforward

# K Fold Cross Validation: -----------------------------------------
# Let's try this first with a mini chunk of the dataset:
warp.all.sp.mini <- warp.all.sp[1:50,]
st_drop_geometry(warp.all.sp.mini)
str(warp.all.sp.mini)

# Setting up a K - fold cross validation model: http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

install.packages("caret")
library(caret)
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(warp.all.sp.mini$encontr_d ~., data = warp.all.sp.mini, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)


# Predict the probabilities based on this model:
glm.probs <- predict(fullmod.covs.glm, type = "response")
glm.probs[1:5]


# Plotting Regression Results ---------------------------------------------

# Plot the regression results:
# Not entirely sure of the best way to do this...
library(ggplot2)
ggplot(covar.glm, aes(x=dom.farms, y=b2pa.distance)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)



# EX From Class Project ---------------------------------------------------
b2pa.distance <- warp.all.sp$ds__PA_
bears_presence <- warp.all.sp$bears

Intercept=10
Slope=30

p=plogis(Intercept+Slope*b2pa.distance)
b.lik=dbinom(x=bears_presence,size = 1,prob = p,log = T)
b.lik_sum=sum(b.lik)
print(b.lik_sum)
# plot(jitter(bears_presence)~b2pa.distance, xlim=c(0,20),xlab="Distance to Nearest Protected Area (km)",ylab="Reported Conflict Point (Bear = 1, Other = 0)")
B1 <- glm(bears_presence~b2pa.distance,family="binomial") # Fit the other variables in here, to run them together in this model

# Log of Plot Distances ---------------------------------------------------
log.dist<-log(b2pa.distance)
plot(jitter(bears_presence)~log.dist,xlab="Log of Distance to Nearest Protected Area (km)",ylab="Reported Grizzly & Black Bear Conflict Points")

p.l=plogis(Intercept+Slope*b2pa.distance)
log.lik=dbinom(x=b2pa.distance,size = 1,prob = p.l,log = T)
log.lik_sum=sum(log.lik)
print(log.lik_sum)
LB1 <- glm(bears_presence~b2pa.distance,family="binomial")
coef(LB1)
curve(plogis(2.444168e-01+ 7.115651e-05*x),add=T,col="blue")

# Creating a quadratic term for the model (since data is bell shap --------
B1.sq <- glm(warp.all.sp$bears~b2pa.distance + I(b2pa.distance^2),family="binomial")

B1 <- glm(bears_presence~b2pa.distance,family="binomial")


# Creating a quadratic term for the model (since data is bell shap --------
B1.sq <- glm(bears.reproj$bears~b2pa.distance + I(b2pa.distance^2),family="binomial")
coef(B1.sq)
summary(B1.sq)
plot(B1.sq)
curve(plogis(-5.770904e-01+-1.740486e-05 *x),add=T,col="blue")

# Results indicate that there is an intercept of 1.492e-01 and slope of ???
# Results indicate that there is an intercept of -6.4327 and slope of 0.000000105

