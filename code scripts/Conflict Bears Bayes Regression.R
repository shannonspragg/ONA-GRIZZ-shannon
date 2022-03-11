# WARP Conflict-only Bayes Regression --------------------------------------
    ### Here we do part two of our modeling - where we do a bayes regression with JUST our conflict points 
#   (1's = bears, 0's = all other species) and then add in our p(general conflict) from the pseudo-abs regression as a predictor

# Load packages:


# And now our just-conflict point data:
warp.df <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP Cropped - SIP/warp_crop_10_ccs.shp")
str(warp.df)

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

warp.df$CCSNAME <- as.factor(warp.df$CCSNAME)
warp.df$CCSUID <- as.factor(warp.df$CCSUID)

CCSUID.co <- warp.df$CCSUID
CCSNAME.co <- warp.df$CCSNAME

which(is.na(warp.df$CCSNAME.ps)) # Like 200 NA's!!
which(is.na(warp.df$CCSUID.ps)) # Like 200 NA's!!

# Add an QUADRATIC term for Farm Count: -----------------------------------
# We want to add a quadratic term to farm count so that we can better interpret it against P(conflict)
total.farms.sq.co <- total.farms.co*total.farms.co



# Fit Data for Rstanarm: --------------------------------------------------

# And do this for our conflict only df:
mini.warp.df.co <- data.frame(bears_presence_co, b2pa.dist.co, b2met.dist.co, total.farms.co, total.farms.co, total.farms.sq.co, dom.farms.co, grizzinc.co, bhs.co, biophys.co, CCSUID.co, CCSNAME.co )


mini.warp.df.co$bears_presence_co <- factor(mini.warp.df.co$bears_presence_co)


# Fit the Regression: -----------------------------------------------------

# Fit our full model with conlfict-only df:
post.co.full <- stan_glmer(bears_presence_co ~ b2pa.dist.co + b2met.dist.co + dom.farms.co + total.farms.co + total.farms.sq.co + (1 | CCSNAME.co), 
                           data = mini.warp.df.co,
                           family = binomial(link = "logit"), # define our binomial glm
                           prior = t_prior, prior_intercept = t_prior, QR=TRUE,
                           seed = SEED, refresh=0) # we add seed for reproducability


