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






# Fit Model with Rstanarm: ------------------------------------------------
library(tidyverse)
install.packages("caret")
library(caret)
library(GGally)
library(ggplot2)
library(corrplot)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
install.packages("rstanarm")
library(rstanarm)
options(mc.cores = 1)
library(loo)
install.packages("projpred")
library(projpred)
SEED=14124869

# Take a look at our data:
summary(warp.df)
mini.warp.df <-  data.frame(bears_presence, b2pa.dist.sc, b2met.dist.sc, total.farms.sq, total.farms.sc, dom.farms, grizzinc.sc, bhs.sc, biophys.sc)


# Make a correlation plot of predictors and outcome:
cor.matrix.df <- data.frame(bears_presence, b2pa.dist.sc, b2met.dist.sc, total.farms.sq, grizzinc.sc, bhs.sc, biophys.sc)

cor.matrix <- cor(cor.matrix.df)
round(cor.matrix, 2)
# Simple correlation plots:
corrplot(cor.matrix, method = 'number', ) # colorful number
corrplot(cor.matrix, addCoef.col = 'black')


# Make our outcome to be factor type and create x and y variables:
mini.warp.df$bears_presence <- factor(mini.warp.df$bears_presence)

# preparing the inputs
x <- model.matrix(bears_presence ~ . - 1, data = mini.warp.df)
y <- mini.warp.df$bears_presence 
y <-formula(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc)


n=dim(mini.warp.df)[1]
p=dim(mini.warp.df)[2]

######## Bayesian logistic regression model:
  # tutorial here: https://avehtari.github.io/modelselection/diabetes.html 
# Set our initial priors to students t with df of 7, and a scale of 2.5 (reasonable default fir prior when coefficients should be close to zero but have some chance of being large:
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# Build our posterior distribution: stan_glm returns the posterior dist for parameters describing the uncertainty related to unknown parameter values
post1 <- stan_glm(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc, data = mini.warp.df,
                  family = binomial(link = "logit"), # define our binomial glm
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = SEED, refresh=0) # we add seed for reproducability

# Plot the posterior for our different variables:
pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)


# Extract the posterior median estimates with the coef function (to get a sense of uncertainty in our estimate):
round(coef(post1), 2)

# Use posterior_interval to get our Bayesian uncertainty intervals
round(posterior_interval(post1, prob = 0.9), 2) # comnpute median and 90% intervals
# interpret this as: we believe that after seeing the data, there is a 0.90 probability that b2pa.dist is between ci90[1,1] and ci90[1,2]



# Leave-one-out Cross_validation: -----------------------------------------

# Loo package implements fast Pareto smoothed leave-one-out-cross-val (PSIS-LOO) to compute expected log predictive density:
(loo1 <- loo(post1, save_psis = TRUE))
# Above we see that PSIS-LOO result is reliable as all Pareto k estimates are small (k< 0.5) Vehtari, Gelman and Gabry (2017a).



# Comparison to Baseline Model: -------------------------------------------

# Compute our baseline result without covariates:
post0 <- update(post1, formula = bears_presence ~ 1, QR = FALSE, refresh=0)

# Compare to our baseline:
(loo0 <- loo(post0)) # computing the PSIS-LOO for our baseline model

loo_compare(loo0, loo1) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions


# Other predictive preformance measures: ----------------------------------

# For more easily interpretable predictive performance measures, 
# we next compute posterior predictive probabilities and use them to compute classification error

# Predicted probabilities
linpred <- posterior_linpred(post1)
preds <- posterior_epred(post1)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2) # gives us 0.11

# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+mean(xor(pr[y==1]<0.5,as.integer(y[y==1]))))/2,2) # gives NAn??

# Let's better estimate the predictive performance for new not yet seen data we next use leave-one-out cross-validation:

# LOO predictive probabilities
ploo=E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post1))$value
# LOO classification accuracy
round(mean(xor(ploo>0.5,as.integer(y==0))),2) # 0.11 result

# LOO balanced classification accuracy
round((mean(xor(ploo[y==0]>0.5,as.integer(y[y==0])))+mean(xor(ploo[y==1]<0.5,as.integer(y[y==1]))))/2,2)

# We can see the small difference in posterior predictive probabilities and LOO probabilities:
qplot(pred, ploo)


########## Calibration of Predictions:

# change y to factor:
y <- as.character(y)

calPlotData<-calibration(y ~ pred + loopred, 
                         data = data.frame(pred=pred,loopred=ploo,y=y), 
                         cuts=10, class="1")
ggplot(calPlotData, auto.key = list(columns = 2))+
  scale_colour_brewer(palette = "Set1")

library(splines)
library(MASS)
ggplot(data = data.frame(pred=pred,loopred=ploo,y=as.numeric(y)-1), aes(x=loopred, y=y)) +
  stat_smooth(method='glm', formula = y ~ ns(x, 5), fullrange=TRUE) +
  geom_abline(linetype = 'dashed') +
  labs(x = "Predicted (LOO)", y = "Observed") +
  geom_jitter(height=0.02, width=0, alpha=0.3) +
  scale_y_continuous(breaks=seq(0,1,by=0.1)) +
  xlim(c(0,1))



# Alternative horseshoe prior on weights: ---------------------------------

# In this example, with n>>p the difference is small, and thus we don’t expect much difference with a 
# different prior and regularized horseshoe prior (Piironen and Vehtari, 2017) is usually more useful for n<p. 

p0 <- 2 # prior guess for the number of relevant variables
tau0 <- p0/(p-p0) * 1/sqrt(n)
hs_prior <- hs(df=1, global_df=1, global_scale=tau0) # create the horseshoe prior - global scale parameter for horseshoe prior is chosen as recommended by Piironen and Vehtari (2017)
t_prior <- student_t(df = 7, location = 0, scale = 2.5) # set our prior again
post2 <- stan_glm(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc, data = mini.warp.df,
                  family = binomial(link = "logit"), 
                  prior = hs_prior, prior_intercept = t_prior,
                  seed = SEED, adapt_delta = 0.999, refresh=0)

# We see that the regularized horseshoe prior has shrunk the posterior distribution of irrelevant features closer to zero, 
# without affecting the posterior distribution of the relevant features.

# plot our posterior: 
pplot <- plot(post2, "areas", prob = 0.95, prob_outer = 1)
pplot + geom_vline(xintercept = 0)

# Pull up the stats:
round(coef(post2), 2)
round(posterior_interval(post2, prob = 0.9), 2)

# We compute LOO also for the model with the regularized horseshoe prior. 
# Expected log predictive density is higher, but not significantly. This is not surprising as this is a easy data with n>>p.
(loo2 <- loo(post2))

loo_compare(loo1, loo2) # our negative value for post2 again shows the predictive value of post1

# Looking at the pairwise posteriors we can see that, for example, IF posteriors for two variable effects are correlating 
# THEN we can’t rely on inferring variable relevance by looking at the marginal distributions.

mcmc_pairs(as.matrix(post2), pars = c("grizzinc.sc","total.farms.sq")) # look at grizzinc and total farms

mcmc_pairs(as.matrix(post2), pars = c("bhs.sc","biophys.sc")) # look at BHS and biophys
# These two actually DO appear slightly negatively correlated --> can't rely on inferring variable relevance by looking at their marginal distributions

mcmc_pairs(as.matrix(post2), pars = c("b2pa.dist.sc","b2met.dist.sc")) # look at dist2PA and dist2Met


########### START HERE #####################
# Projection predictive variable selection: -------------------------------
  # Next we do variable selection using projection predictive variable selection

varsel2 <- cv_varsel(post2, method='forward', cv_method='loo', nloo = n)

# We get a LOO based recommendation for the model size and the selected variables
(nsel<-suggest_size(varsel2))

(vsel<-solution_terms(varsel2)[1:nsel])

# We can now look at the estimated predictive performance of smaller models compared to the full model:

plot(varsel2, stats = c('elpd', 'pctcorr'), deltas=FALSE)





