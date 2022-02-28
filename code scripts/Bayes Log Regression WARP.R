# WARP Conflict Bayesian Logistic Regression ------------------------------------------------
# This script will be where I import the WARP data and do a mixed affects logistic regression with lme4
# Important Questions -- NA


# Load Packages -----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)

library(ggplot2)
#install.packages("GGally")
library(GGally)
#install.packages("reshape2")
library(reshape2)
library(lme4)
library(compiler)
library(parallel)
library(boot)
library(lattice)

# Import the All Species Master df for our Southern Interior EcoProvince ----------------------------------------

warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10km_buf.shp")


# Scale the Variables: ----------------------------------------------------------

# We can use the scale function to center our predictors by subtracting the means (center= TRUE) and scaling by dividing by their standard deviation (scale=TRUE)
# Here we create a function to scale by subtracting the mean and dividing by 2 standard deviations:
scale2sd <-function(variable){(variable - mean(variable, na.rm=TRUE))/(2*sd(variable, na.rm=TRUE))}

b2pa.dist.sc <- scale2sd(warp.df$ds__PA_)
total.farms.sc <- scale2sd(warp.df$Ttl_F_C)
b2met.dist.sc <- scale2sd(warp.df$dstn___)
grizzinc.sc <- scale2sd(warp.df$GrzzInE)
bhs.sc <- scale2sd(warp.df$BHSExtr)
biophys.sc <- scale2sd(warp.df$BphysEx)

bears_presence <- warp.df$bears # Binomial bears
dom.farms <- warp.df$Dm_Fr_T # Dominant farm type covariate -- non numeric
dom.farms <- as.factor(warp.df$Dm_Fr_T) # Making this work as a factor

# Add an QUADRATIC term for Farm Count: -----------------------------------
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq <- total.farms.sc*total.farms.sc


# Visualize the Data ------------------------------------------------------


ggpairs(warp.df[, c("ds__PA_", "dstn___", "Ttl_F_C", "GrzzInE", "BHSExtr", "BphysEx")])


# Run Initial Model: ------------------------------------------------------

# estimate the model and store results in m
full.mod.1 <- glmer(bears ~ b2pa.dist.sc + total.farms.sc + total.farms.sq + dom.farms + b2met.dist.sc + bhs.sc + biophys.sc +
             (1 | total.farms.sq), data = warp.df, family = binomial, control = glmerControl(optimizer = "bobyqa"), #avoid warnings of nonconvergance
           nAGQ = 10)

# print the mod results without correlations among fixed effects
print(full.mod.1, corr = FALSE)
summary(full.mod.1) # estimates show our regression coefficients, 

CI.mod1 <- sqrt(diag(vcov(full.mod.1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(full.mod.1), LL = fixef(full.mod.1) - 1.96 * CI.mod1, UL = fixef(full.mod.1) + 1.96 *
                CI.mod1))
# table of odds ratios (instead of coefficients on the logit scale):
exp(tab)


# Bootstrapping: ----------------------------------------------------------

sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

# Resample data and take 100 replocates
set.seed(20)
tmp <- sampler(warp.df, reps = 100)
bigdata <- cbind(tmp, hdp[tmp$RowID, ])
