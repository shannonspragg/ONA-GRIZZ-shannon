# WARP Conflict Binomial Linear Regression ------------------------------------------------
# This script will be where I import the all species master df and configure it into the binomial 
# linear regression analysis code
  # Important Questions -- how do I determine what slope & intercept makes sense??
  # Do I preform the binomial regression for each variable individually? or all together in the code?


# Import the All Species Master df ----------------------------------------

warp.all.sp <- st_read("/Users/shannonspragg/ONA_GRIZZ/WARP Bears /WARP All Species Full Yr/ WARP All Species Master Data Frame.shp")

# Create Binomial GLM -- Dist to PA Variable -----------------------------------------------------
b2pa.distance <- warp.all.sp$ds__PA_
bears_presence <- warp.all.sp$bears

Intercept=10
Slope=30

p=plogis(Intercept+Slope*b2pa.distance)
b.lik=dbinom(x=bears_presence,size = 1,prob = p,log = T)
b.lik_sum=sum(b.lik)
print(b.lik_sum)
plot(jitter(bears_presence)~b2pa.distance, xlim=c(0,20),xlab="Distance to Nearest Protected Area (km)",ylab="Reported Conflict Point (Bear = 1, Other = 0)")
B1 <- glm(bears_presence~b2pa.distance,family="binomial")


# Creating a quadratic term for the model (since data is bell shap --------
B1.sq <- glm(bears.reproj$bears~b2pa.distance + I(b2pa.distance^2),family="binomial")
coef(B1.sq)
summary(B1.sq)
plot(B1.sq)
curve(plogis(-5.770904e-01+-1.740486e-05 *x),add=T,col="blue")

# Results indicate that there is an intercept of -6.4327 and slope of 0.000000105


