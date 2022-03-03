# WARP Conflict Bayesian Logistic Regression ------------------------------------------------
# This script will be where I import the WARP data and do a mixed affects logistic regression with rstanarm
# Important Questions -- Is it ok to use the student-t prior? 


# Load Packages -----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)

library(tidyverse)
#install.packages("caret")
library(caret)
library(GGally)
library(ggplot2)
library(corrplot)
library(bayesplot)
theme_set(bayesplot::theme_default(base_family = "sans"))
#install.packages("rstanarm")
library(rstanarm)
options(mc.cores = 1)
library(loo)
#install.packages("projpred")
library(projpred)
SEED=14124869

#install.packages("sjPlot")
library(sjPlot)
#install.packages("nloptr")
library(nloptr)
#install.packages("sjmisc")
library(sjmisc)
#install.packages("rsq")
library(rsq)


# Import the All Species Master df for our Southern Interior EcoProvince ----------------------------------------

#warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10km_buf.shp")
warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crom_10_ccs.shp")
str(warp.df)

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

warp.df$CCSNAME <- as.factor(warp.df$CCSNAME)
warp.df$CCSUID <- as.factor(warp.df$CCSUID)

CCSUID <- warp.df$CCSUID
CCSNAME <- warp.df$CCSNAME
# Add an QUADRATIC term for Farm Count: -----------------------------------
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq <- total.farms.sc*total.farms.sc


# Visualize the Data ------------------------------------------------------

boxplot(total.farms.sq ~ dom.farms, data = warp.df)

boxplot(grizzinc.sc ~ dom.farms, data = warp.df)
boxplot(b2pa.dist.sc ~ dom.farms, data = warp.df)


# Fit Model with Rstanarm: ------------------------------------------------

# Take a look at our data:
summary(warp.df)
mini.warp.df <-  data.frame(bears_presence, b2pa.dist.sc, b2met.dist.sc, total.farms.sq, total.farms.sc, dom.farms, grizzinc.sc, bhs.sc, biophys.sc, CCSUID, CCSNAME)

############ Make a correlation plot of predictors and outcome:
cor.matrix.df <- data.frame(bears_presence, b2pa.dist.sc, b2met.dist.sc, total.farms.sc , total.farms.sq, grizzinc.sc, bhs.sc, biophys.sc)

cor.matrix <- cor(cor.matrix.df)
round(cor.matrix, 2)
# Simple correlation plots:
corrplot(cor.matrix, method = 'number', ) # colorful number
corrplot(cor.matrix, addCoef.col = 'black')

# Make a plot with proportional circles on a diagonal, coefficent numbers, and legend at the bottom:
predictor.cor.plot <- corrplot(cor.matrix, type = 'lower', order = 'hclust', tl.col = 'black',addCoef.col = 'dark grey',
                               cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10))

############# Make our outcome to be factor type and create x and y variables:
mini.warp.df$bears_presence <- factor(mini.warp.df$bears_presence)

# preparing the inputs
x <- model.matrix(bears_presence ~ . - 1, data = mini.warp.df)
y <- mini.warp.df$bears_presence 
y <-formula(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc)


n=dim(mini.warp.df)[1]
p=dim(mini.warp.df)[2]


# Fitting our Posterior Regression: ---------------------------------------
  # tutorial here: https://avehtari.github.io/modelselection/diabetes.html 

# Set our initial priors to students t with df of 7, and a scale of 2.5 (reasonable default fir prior when coefficients should be close to zero but have some chance of being large:
t_prior <- student_t(df = 7, location = 0, scale = 2.5)

# Build our posterior distribution: stan_glm returns the posterior dist for parameters describing the uncertainty related to unknown parameter values
post1 <- stan_glm(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc, data = mini.warp.df,
                  family = binomial(link = "logit"), # define our binomial glm
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = SEED, refresh=0) # we add seed for reproducability

##### Add in a Varying Intercept for SOI CCS Region:
post1.var.int <- stan_glmer(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc + (1 | CCSNAME), data = mini.warp.df,
                  family = binomial(link = "logit"), # define our binomial glm
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                  seed = SEED, refresh=0) # we add seed for reproducability




############# Plot the posterior for our different variables:
pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

# Try this with the random intercept:
pplot<-plot(post1.var.int, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)


############## Posterior Coefficients & Intervals:
# Extract the posterior median estimates with the coef function (to get a sense of uncertainty in our estimate):
round(coef(post1), 2)


# Make a table with coefficients:
sjPlot::tab_model(post1)
summ(post1)
(coef(post1))

# Use posterior_interval to get our Bayesian uncertainty intervals
round(posterior_interval(post1, prob = 0.9), 2) # comnpute median and 90% intervals
# interpret this as: we believe that after seeing the data, there is a 0.90 probability that b2pa.dist is between ci90[1,1] and ci90[1,2]


# Leave-one-out Cross_validation: -----------------------------------------

############## Run a Leave-One-Out (LOO):
# Loo package implements fast Pareto smoothed leave-one-out-cross-val (PSIS-LOO) to compute expected log predictive density:
(loo1 <- loo(post1, save_psis = TRUE))
# Above we see that PSIS-LOO result is reliable as all Pareto k estimates are small (k< 0.5) Vehtari, Gelman and Gabry (2017a).

plot(loo1, label_points = TRUE)
loo1 # get the summary of our test

############## Comparison to Baseline Model: -------------------------------------------

# Compute our baseline result without covariates:
post0 <- update(post1, formula = bears_presence ~ 1, QR = FALSE, refresh=0)

# Compare to our baseline:
(loo0 <- loo(post0)) # computing the PSIS-LOO for our baseline model

loo_compare(loo0, loo1) # this high negative value for post0 shows us the covariates contain clearly useful information for predictions


############## Other predictive performance measures: ----------------------------------

# For more easily interpretable predictive performance measures, 
# we next compute posterior predictive probabilities and use them to compute classification error

# Predicted probabilities
linpred <- posterior_linpred(post1)
preds <- posterior_epred(post1)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

med.preds <- apply(preds, 2, median)
pr.1 <- median(pred) # extract the median estimate of our posterior probabilites

# Let's better estimate the predictive performance for new not yet seen data we next use leave-one-out cross-validation:

# LOO predictive probabilities
ploo=E_loo(preds, loo1$psis_object, type="mean", log_ratios = -log_lik(post1))$value

# compute LOO classification error
round(mean(xor(ploo>0.5,as.integer(y==0))),2) # 0.11 result

# LOO balanced classification accuracy
round((mean(xor(ploo[y==0]>0.5,as.integer(y[y==0])))+mean(xor(ploo[y==1]<0.5,as.integer(y[y==1]))))/2,2)


#### Plot Differences -- posterior predictive probs vs LOO probs:
# We can see the very small difference in posterior predictive probabilities and LOO probabilities:
qplot(pred, ploo)


#  Find area under the curve & Compare Posterior Predictions: ---------------------------------------------
#install.packages("pROC")
library(pROC)
library(bayestestR)
#install.packages("randomForest")
library(randomForest)

area_under_curve(med.preds, linpred, method = "trapezoid")

# Running example -- obese = bears_pres
# Make glm model for the regression
stan.glm.fit <- stan_glm(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc, data = mini.warp.df,
                         family = binomial(link = "logit"))

# Model for the posterior1
stan.glm.fit1 <- stan_glm(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc, data = mini.warp.df,
                             family = binomial(link = "logit"), # define our binomial glm
                             prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                             seed = SEED, refresh=0) # we add seed for reproducability

post.var.int.fit <- stan_glm(bears_presence ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc + (1 | CCSNAME), 
                               data = mini.warp.df,
                               family = binomial(link = "logit"), # define our binomial glm
                               prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                               seed = SEED, refresh=0) # we add seed for reproducability

# Plot ROC:
par(pty="s") # sets our graph to square
roc(bears_presence, stan.glm.fit1$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 conrols (bears 0) < 2062 cases (bears 1), Area under curve = 0.6547

# Calculate partial AUC on graph:
roc(bears_presence, stan.glm.fit1$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE, 
    print.auc.x=45 ,  # specify where on x axis we want AUC to be printed (so as not to block anything)
    partial.auc=c(100,80),  # 0-20% false positive -- the range of specificity values we want to focus on (100% specificity = 0% on our 1-Specificity axis)
    auc.polygon = TRUE,   # draw the partial auc
    auc.polygon.col = "#377eb822") # adding 22 makes it semi-transparent

# Plot ROC for our Random Intercept Model:
par(pty="s") # sets our graph to square
roc(bears_presence, post.var.int.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 conrols (bears 0) < 2062 cases (bears 1), Area under curve = 0.6547



############### Overlap 2 ROC curves:

# Make random forest model with regression:
rf.model <- randomForest(factor(bears_presence) ~ b2pa.dist.sc + b2met.dist.sc + dom.farms + total.farms.sc + total.farms.sq + grizzinc.sc + bhs.sc + biophys.sc)

# Plot ROC curve for our regression:
roc(bears_presence, stan.glm.fit1$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE ,
    xlab= "False Positive Percentage", ylab= "True Positive Percentage",
    col="#377eb8", lwd=4, print.auc=TRUE) # this gives us the ROC curve , in 3544 conrols (bears 0) < 2062 cases (bears 1), Area under curve = 0.6547

# Add ROC curve for random forest
plot.roc(bears_presence, rf.model$votes[,1], percent=TRUE, col='#4daf4a', lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)

legend("bottomright", legend=c("Bayes Logistic Regression", "Random Forest"),
       col=c("#377eb8", "#4daf4a"), lwd = 4)
#******* What this tells us: a 65% discrimination (model predictions are correct 65% of the time) is not great. (50% = no discrimination).
#* So we want to shoot for a better model --> one with 75-90% discrimination if we can get it.


############### Look at range of thresholds for section of curve:
roc.info <- roc(bears_presence, stan.glm.fit$fitted.values, legacy.axes=TRUE)
roc.df <- data.frame(     # make a df with our roc values
  tpp= roc.info$sensitivities*100,  # include all our true positive percentages (by multiplying by 100)
  fpp=(1 - roc.info$specificities)*100,  # and all our false positive percentages (by multiplying 1- specificity by 100)
  thresholds= roc.info$thresholds)

head(roc.df) # we see when threshold is -INF , the true positive % (tpp) is 100 (bc all samples that were bears were correctly classified)
# this first row in the df corresponds to the uppermost right corner of the curve
tail(roc.df) # this last row corresponds to the bottom left corner of the curve

# Select the range of values when the true positive rate is between 60-80%
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

# compute posterior classification error
round(mean(xor(pr,as.integer(y==0))),2) # gives us 0.11

# posterior balanced classification accuracy
round((mean(xor(pr[y==0]>0.5,as.integer(y[y==0])))+mean(xor(pr[y==1]<0.5,as.integer(y[y==1]))))/2,2) # gives NAn??

# Plotting our Posterior & Chains: ----------------------------------------

############### Let's look at all our parameters together in a paiwise plot:
color_scheme_set("pink")
mcmc_pairs(as.matrix(post1), pars = c("(Intercept)", "b2pa.dist.sc","b2met.dist.sc","bhs.sc","biophys.sc", "grizzinc.sc","total.farms.sq" ),
           off_diag_args = list(size= 1.5)) # look at dist2PA and dist2Met

############### Plot our MCMC chains mixing:
color_scheme_set("mix-blue-red")
mcmc_trace(post1, pars = c("b2pa.dist.sc","b2met.dist.sc","bhs.sc","biophys.sc", "grizzinc.sc","total.farms.sq"), 
           facet_args = list(ncol = 1, strip.position = "left"))

# One with just the ag variables:
mcmc_trace(post1, pars = c("dom.farmsVegetable and melon farming [1112]" , "dom.farmsCattle ranching and farming [1121]", "dom.farmsOther crop farming [1119]" ,"total.farms.sq"), 
           facet_args = list(ncol = 1, strip.position = "left"))

############### Plot MCMC posterior areas:
mcmc_areas(as.matrix(post1), prob = 0.95, prob_outer = 1)

# Posterior parameter histograms:
mcmc_hist(post2)
mcmc_hist_by_chain(post2, pars = c("dom.farmsVegetable and melon farming [1112]"))

############## Plot Posterior Mixed Effects:
# Basic Mixed Effect Plot:
sjPlot::plot_model(post1)

# Labeled Effect Plot:
# Notes: axis labels should be in order from bottom to top. 
# To see the values of the effect size and p-value, set show.values and show.p= TRUE
post1.effects.plot <- sjPlot::plot_model(post1, 
                   axis.labels=c("CS Biophysical (HII + topo ruggedness)","Bear Habitat Suitability", "Grizz Increase", "Total Farm Count ^2" ,"Total Farm Count" ,"Vegetable & Melon Farming" ,
                                 "Other Crop Farming (tobacco, peanut, sugar-cane, hay, herbs & spices) ", "Other Animal Production (bees & honey, equine, fur-bearers)" , "Fruit & Tree Nut Farming" , "Cattle Ranching & Farming" ,
                                   "Distance to Metro Area (km)" , "Distance to Protected Area (km)"),
                   show.values=TRUE, show.p=FALSE,
                   title="Effects of Social & Environmental Variables on Bear Conflict")

##### Model Table of Effect Size:
sjPlot::tab_model(post1)

# Formatted Table (with labels):
# Notes: predictor labels (pred.labels) should be listed from top to bottom; dv.labels= the name of the response variable that will be at the top of the table.

post1.effects.tab <- sjPlot::tab_model(post1, 
                  show.re.var= TRUE, show.se = TRUE , show.p = TRUE , show.r2 = TRUE, show.aic = TRUE , 
                  pred.labels =c("(Intercept)" , "Distance to Protected Area (km)", "Distance to Metro Area (km)", "Cattle Ranching & Farming" , "Fruit & Tree Nut Farming" ,"Other Animal Production (bees & honey, equine, fur-bearers)" ,
                                 "Other Crop Farming (tobacco, peanut, sugar-cane, hay, herbs & spices) ", "Vegetable & Melon Farming" , "Total Farm Count" , "Total Farm Count ^2" , "Grizz Increase", "Bear Habitat Suitability", "CS Biophysical (HII + topo ruggedness)"),
                                 dv.labels= "Effects of Social & Environmental Variables on Bear Conflict")




# Calibration of Predictions: ---------------------------------------------

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




# Alternative horseshoe prior on weights: --------------------------------------------------------

################## Using a Horseshoe Prior:
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

############# Run a LOO for our new posterior:
# We compute LOO also for the model with the regularized horseshoe prior. 
# Expected log predictive density is higher, but not significantly. This is not surprising as this is a easy data with n>>p.
(loo2 <- loo(post2))

loo_compare(loo1, loo2) # our negative value for post2 again shows the predictive value of post1


############### Plot out MCMC Pairwise Parameters:
# Looking at the pairwise posteriors we can see that, for example, IF posteriors for two variable effects are correlating 
# THEN we can’t rely on inferring variable relevance by looking at the marginal distributions.

mcmc_pairs(as.matrix(post2), pars = c("grizzinc.sc","total.farms.sq")) # look at grizzinc and total farms

mcmc_pairs(as.matrix(post2), pars = c("bhs.sc","biophys.sc")) # look at BHS and biophys
# These two actually DO appear slightly negatively correlated --> can't rely on inferring variable relevance by looking at their marginal distributions


############### Let's plot all our parameters together:
color_scheme_set("pink")
mcmc_pairs(as.matrix(post2), pars = c("(Intercept)", "b2pa.dist.sc","b2met.dist.sc","bhs.sc","biophys.sc", "grizzinc.sc","total.farms.sq" ),
           off_diag_args = list(size= 1.5)) # look at dist2PA and dist2Met

############## Plot our MCMC chains mixing:
color_scheme_set("mix-blue-red")
mcmc_trace(post2, pars = c("b2pa.dist.sc","b2met.dist.sc","bhs.sc","biophys.sc", "grizzinc.sc","total.farms.sq"), 
           facet_args = list(ncol = 1, strip.position = "left"))

mcmc_trace(post2, pars = c("dom.farmsVegetable and melon farming [1112]" , "dom.farmsCattle ranching and farming [1121]", "dom.farmsOther crop farming [1119]" ,"total.farms.sq"), 
           facet_args = list(ncol = 1, strip.position = "left"))

############## Let's see our posterior areas:
mcmc_areas(as.matrix(post2), prob = 0.95, prob_outer = 1)

# Posterior parameter histograms:
mcmc_hist(post2)
mcmc_hist_by_chain(post2, pars = c("dom.farmsVegetable and melon farming [1112]"))



############## Projection predictive variable selection: -------------------------------
  # Next we do variable selection using projection predictive variable selection

varsel2 <- cv_varsel(post1, method='forward', cv_method='loo', nloo = n)

# We get a LOO based recommendation for the model size and the selected variables
(nsel<-suggest_size(varsel2))

(vsel<-solution_terms(varsel2)[1:nsel]) # this shows us the recommended variables

# We can now look at the estimated predictive performance of smaller models compared to the full model:

plot(varsel2, stats = c('elpd', 'pctcorr'), deltas=FALSE)

# Next we form the projected posterior for the chosen model.

proj2 <- project(varsel2, nv = nsel, ns = 4000)
proj2draws <- as.matrix(proj2)
colnames(proj2draws) <- c("Intercept",vsel)
round(colMeans(proj2draws),1) # this gives us the means for our variables

# Pull up the interval for our posterior:
round(posterior_interval(proj2draws),1)

# Plot our mcmc areas: The projected posterior can be used to make predictions in the future (with no need to measure the left out variables).

mcmc_areas(proj2draws, prob = 0.95, prob_outer = 1,
           pars = c('Intercept', vsel))




