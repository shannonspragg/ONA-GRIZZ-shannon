# WARP Conflict Binomial Linear Regression ------------------------------------------------
# This script will be where I import the all species master df and configure it into the binomial 
# linear regression analysis code
  # Important Questions -- NA


# Load Packages -----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)

# Import the All Species Master df ----------------------------------------

warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")

# Create Binomial GLM -- Bring in All Covariates -----------------------------------------------------
bears_presence <- warp.all.sp$bears # Binomial bears
b2pa.distance <- scale(warp.all.sp$ds__PA_) # Dist to PA covariate
dom.farms <- warp.all.sp$Dmn_F_T # Dominant farm type covariate
total.farms <- warp.all.sp$Ttl_F_C # Total farm count covariate
b2met.dist <- warp.all.sp$dstn___ # Dist to metro covariate


# Prep Simulation to Match Data with Binomial Reg: ------------------------
# Creating a simulation to see if it returns close to the intercept and slope we input initially 
# https://daviddalpiaz.github.io/appliedstats/logistic-regression.html

Intercept=0.3 # This is set for a 0-1 value, representing bear presence
Slope=2 # need a slope for each variable, so we create a matrix for each variable 
    # Do this like this: slope (B) <- runif(15,-2,2) https://data.library.virginia.edu/simulating-a-logistic-regression-model/
B_slope <- runif(10, -2,2) # Created a randomized slope matrix for the variables

p=plogis(Intercept+Slope*b2pa.distance) # simulating p with our intercept and slope, to fit model to p

y_sim <- rbinom(length(bears_presence), 1, prob = p) # running the simulation with probability p
glm(y_sim ~ b2pa.distance, family = "binomial") # running the regression based on the simulation



# Example from Class Project:

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
=======
B1 <- glm(bears_presence~b2pa.distance,family="binomial")


# Creating a quadratic term for the model (since data is bell shap --------
B1.sq <- glm(bears.reproj$bears~b2pa.distance + I(b2pa.distance^2),family="binomial")
>>>>>>> c90dfc9451abe0becf3e42c4ae48007617546c91
coef(B1.sq)
summary(B1.sq)
plot(B1.sq)
curve(plogis(-5.770904e-01+-1.740486e-05 *x),add=T,col="blue")

<<<<<<< HEAD
# Results indicate that there is an intercept of 1.492e-01 and slope of ???
=======
# Results indicate that there is an intercept of -6.4327 and slope of 0.000000105
>>>>>>> c90dfc9451abe0becf3e42c4ae48007617546c91


