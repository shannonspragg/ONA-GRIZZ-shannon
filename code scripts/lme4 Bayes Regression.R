
# Bayes Regression with lme4: ---------------------------------------------

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

boxplot(total.farms.sq ~ dom.farms, data = warp.df)

boxplot(grizzinc.sc ~ dom.farms, data = warp.df)
boxplot(b2pa.dist.sc ~ dom.farms, data = warp.df)



# Fit Model with lme4: ----------------------------------------------------
library(lme4)
#install.packages("lmerTest")
library(lmerTest)

#fit a model with random effects and add random intercepts:
# NOTE: random effects are typically used for our categorical variable - meaning the intercept or slope varies by category
full.mod.1 <- glmer(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + total.farms.sc + total.farms.sq + dom.farms + grizzinc.sc
                    + bhs.sc + biophys.sc + (1|dom.farms), family = binomial, data=warp.df) #without interaction


# Sub Models:
ecol.mod <- glmer(bears_presence ~ biophys.sc + bhs.sc , family = binomial, data = warp.df)

summary(full.mod.1)
plot(full.mod.1)


# Adding Interaction Terms ------------------------------------------------
# Add some interaction terms - just to see:
full.mod.int <- glmer(bears_presence ~ b2pa.dist.sc * b2met.dist.sc + total.farms.sc + total.farms.sq + dom.farms + grizzinc.sc
                      + bhs.sc + biophys.sc + (1|dom.farms),family = binomial, data=warp.df) #with interaction for PA's 
full.mod.int2 <- glmer(bears_presence ~ b2pa.dist.sc * b2met.dist.sc + total.farms.sq + total.farms.sc * dom.farms + grizzinc.sc
                       + bhs.sc + biophys.sc + (1|dom.farms),family = binomial, data=warp.df) #with interaction for PA's AND farms


# Compare the two models:
anova(full.mod.1, full.mod.int) # the model with interaction IS slightly lower than that without one
anova(full.mod.int, full.mod.int2) # looks like model with both interaction terms is best AIC --> could just be due to added terms


# Try adding a random slope to the model:
mod.int.r <- lmer(bears_presence ~ b2pa.dist.sc * b2met.dist.sc + total.farms.sq + total.farms.sc * dom.farms + grizzinc.sc
                  + bhs.sc + biophys.sc + (biophys.sc|total.farms.sc),data=warp.df)
anova(full.mod.int2,mod.int.r) # don't really need this random slope, makes AIC increase

# Back to our better model with random intercepts:
anova(full.mod.int2) # analysis of variance table
# we can see the effects of most of our variables are "significant", and our interaction terms




# Visualize our Model: ----------------------------------------------------
#install.packages("sjPlot")
library(sjPlot)
#install.packages("nloptr")
library(nloptr)
#install.packages("sjmisc")
library(sjmisc)
#install.packages("rsq")
library(rsq)

# Basic Mixed Effect Plot:
sjPlot::plot_model(full.mod.int2)


##### Model Table of Effect Size:
sjPlot::tab_model(full.mod.int2)



# Bootstrapping to Resample: ----------------------------------------------
# Let's get predictions for all values of bear conflict report presence
newdat <- subset(warp.df,dom.farms=="Vegetable and melon farming [1112]")
bb <- bootMer(full.mod.int2, FUN=function(x)predict(x, newdat, re.form=NA),
              nsim=999)

#extract the quantiles and the fitted values.
lci <- apply(bb$t, 2, quantile, 0.025)   
uci <- apply(bb$t, 2, quantile, 0.975)   
pred <- predict(full.mod.int2,newdat,re.form=NA)

# Plot our confidence intervals:
library(scales)
palette(alpha(c("blue","red","forestgreen","darkorange"),0.5))
plot(grizzinc.sc~jitter(total.farms.sc),col=dom.farms,data=warp.df[warp.df$Dm_Fr_T=="Vegetable and melon farming [1112]",],pch=16)
lines(pred~grizzinc.sc,newdat,lwd=2,col="orange",alpha=0.5)
lines(lci~grizzinc.sc,newdat,lty=2,col="orange")
lines(uci~grizzinc.sc,newdat,lty=2,col="orange")





