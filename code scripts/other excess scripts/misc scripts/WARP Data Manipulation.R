
# WARP Data Prep ----------------------------------------------------------
library(tidyverse)
library(dplyr)
install.packages("raster")
library("raster")

conflict.data<-read.csv("Data/WARP 6mo all species.csv")
head(conflict.data)

# Merge the two encounter columns into one total column
conflict.data$total_encounter<-conflict.data$encounter_adults + conflict.data$encounter_young
head(conflict.data)


# Convert selected species to 1's and all others to 0's:

bears.conflict<- conflict.data %>% 
  mutate(conflict.data, bears = if_else(species_name == "BLACK BEAR" | species_name == "GRIZZLY BEAR", 1, 0))
head(bears.conflict)

# Steps to Create a Distance to PA variable:
install.packages("sf")
installed.packages("sp")
install.packages("rgdal")
library(rgdal)
library("sf")
library("sp")

# Making Conflict Data a Spatial Dataframe --------------------------------

bc.sp<-structure(bears.conflict,longitude= "encounter_lng", latitude= "encounter_lat", class="data.frame")
head(bc.sp)
xy<-bc.sp[,c(8,7)]
bears.spdf<-SpatialPointsDataFrame(coords = xy,data = bc.sp,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
str(bears.spdf)

bears.sf <- as(bears.spdf, "sf")


# Load in Canada Spatial Data ---------------------------------------------
library(rgdal)
library(sf)
library(tidyverse)
fgdb <- "/Users/shannonspragg/Downloads/CPCAD-BDCAPC_Dec2020.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class for PA shapes
fc <- readOGR(dsn=fgdb,layer="CPCAD_Dec2020")
fc.sf <- as(fc, "sf") 
bc.ab.PA <- fc.sf %>% 
  filter(., LOC_E == "British Columbia" | LOC_E == "Alberta") %>% 
  st_make_valid()
st_write(bc.ab.PA,"bc_ab_PAs.shp")
# Now that this is written as a .shp, we will just import it here:
bc.ab.PA<-st_read("bc_ab_PAs.shp")

bc.PAs <- fc.sf %>% 
  filter(., LOC_E == "British Columbia" | LOC_E == "Alberta") %>% 
  st_make_valid()
st_write(bc.PAs,"bc_PAs.shp")
plot(st_geometry(bc.ab.PA))

# Read in the .shp for Canada census tracts data:
cen.tracts<-st_read("Data/Census Tracts/lct_000b16a_e.shp")

# Read in the .shp for Canada FSA boundaries data:
can.fsa<-st_read("Data/lfsa000b16a_e (1)/lfsa000b16a_e.shp")


# Make sure it is an sf object
ct.sf<- as(cen.tracts, "sf")
bc.ab.cen.tracts<-ct.sf %>%
  filter(., PRNAME == "British Columbia" | PRNAME == "Alberta") %>%
  st_make_valid()

# Make sure it is an sf object
fsa.sf<- as(can.fsa, "sf")
str(fsa.sf)

bc.ab.fsa<-fsa.sf %>%
  filter(., PRNAME == "British Columbia / Colombie-Britannique" | PRNAME == "Alberta") %>%
  st_make_valid()
str(bc.ab.fsa)

unique(fsa.sf$PRNAME)
unique(bc.ab.fsa$PRNAME)

# Trying to determine how many unique ID characters are in these columns 
unique(as.character(bc.ab.cen.tracts$CTUID)) # 631 CTUIDs
unique(as.character(bc.ab.cen.tracts$CMAUID)) # 7 CMAUIDs
unique(as.character(bc.ab.cen.tracts$CMAPUID)) # 7 CMAPUIDs

# Check for any NA values in the column
nrow(bc.ab.cen.tracts[is.na(bc.ab.cen.tracts$CMAUID),])

head(bc.ab.cen.tracts, n=15)

# Re-project the Data ------------------------------------------------------

# Canada_Albers_Equal_Area_Conic:
bears.reproj <- st_transform(bears.sf, st_crs(bc.ab.PA))
plot(st_geometry(bc.ab.fsa))
st_crs(bears.reproj)

fsa.reproj <- st_transform(bc.ab.fsa, st_crs(bc.ab.PA))
st_crs(fsa.reproj)

# PCS_Lambert_Conformal_Conic:
bears.reproj.2 <- st_transform(bears.reproj, st_crs(bc.ab.cen.tracts))
str(bears.reproj.2)
st_crs(bears.reproj.2) #Now bears.reproj.2 is ALSO in PCS_Lambert_Conformal_Conic

str(bc.ab.cen.tracts) # This is in PCS_Lambert_Conformal_Conic
head(bc.ab.cen.tracts)
census.reproj <- st_transform(bc.ab.cen.tracts, st_crs(bc.ab.PA)) # I think this is projecting wrong...
head(census.reproj)

st_crs(census.reproj)
st_crs(bears.reproj)
# Write the bears and census main files into .csv so this is reproducible
write.csv(bears.reproj,"Data/bears.reproj.csv")
write.csv(bc.ab.cen.tracts,"Data/bc.ab.cen.tracts.csv") # This .csv file keeps distorting (looks fine in R, but not when exported)

# Create Distance of Conflict Points to P.A's -----------------------------
#calculation of the distance between the PA's and our points

# dist[1:9, 1:9] 
dist.pts2pas <- st_distance(bears.reproj, bc.ab.PA)
head(dist.pts2pas)
# Must find the minimum distance to PA's (Distance from conflict point to nearest PA)
min.dist <- apply(dist.pts2pas, 1, min)
str(min.dist)

# Add Distance Variable into Datatable -----------------------------------

bears.reproj$distance_to_PAs<-min.dist
head(bears.reproj)
write_csv(bears.reproj,"bears_w_distance.csv")
# Remove the units from the values (note: in meters)
as.numeric(bears.reproj$distance_to_PAs)
# Convert units from meters to km:
install.packages("measurements")
library(measurements)

dist_in_km<-conv_unit(bears.reproj$distance_to_PAs,"m","km")
str(dist_in_km)
bears.reproj$distance_to_PAs<-dist_in_km
head(bears.reproj)
write_csv(bears.reproj,"bears_w_distance.csv")


# Spatial Join of Bears to Census Tracts ----------------------------------
# This is just what I've been messing around with, the following chunk has not 
# created a successful join yet
bears.PRUID.join<- st_join(bears.reproj,fsa.reproj["PRUID"])
bears.full.join. <- st_join(bears.reproj,fsa.reproj)

unique(bears.full.join.$CFSAUID) # There are 163 FSA districts
# Trying Join with sp not sf ----------------------------------------------
library("sp")
library("rgeos")
bears.sp<-as(bears.reproj, Class= "Spatial")
str(bears.sp)
census.sp<-as(census.reproj, Class= "Spatial")
str(census.sp)

#Create Buffer around spatial points:
bears_buf_sp<- gBuffer(bears.sp, width = 2000) # create buffer around center

bears_buf_intersects <-  gIntersects (bears_buf_sp, census.sp, byid=TRUE)

# what kind of object is this?
class(bears_buf_intersects)
# subset
bears_sel_sp <- census.sp[as.vector(bears_buf_intersects),]
# plot
plot (census.sp, border="#aaaaaa")
plot (bears_sel_sp, add=T, col="red") 
plot (bears_buf_sp, add=T, lwd = 2)

# Trying join with sf not sp ----------------------------------------------
library("sf")
bears.sf<-as(bears.reproj, Class = "sf")
bc.fsa.sf<-as(fsa.reproj, Class = "sf")
# Create a 2km buffer around center points
bears_buf_sf <- st_buffer(bears.sf, 2000)

bears_buf_intersects <- st_intersects(bears_buf_sf, bc.fsa.sf)
st_crs(bears_buf_sf)
st_crs(bc.fsa.sf) # Same CRS for both
class(bears_buf_intersects)

bears_sel_sf <- bc.fsa.sf[bears_buf_intersects[[1]],]

# plot
plot(st_geometry(bc.fsa.sf), border="#aaaaaa", main="Census tracts that fall within 2km of city center")
plot(st_geometry(bears_sel_sf), add=T, col="red")
plot(st_geometry(bears_buf_sf), add=T, lwd = 2)


# Create Binomial GLM -----------------------------------------------------
b2pa.distance<-bears.full.join.$distance_to_PAs
bears_presence<-bears.reproj$bears

Intercept=10
Slope=30

p=plogis(Intercept+Slope*b2pa.distance)
b.lik=dbinom(x=bears_presence,size = 1,prob = p,log = T)
b.lik_sum=sum(b.lik)
print(b.lik_sum)
plot(jitter(bears_presence)~b2pa.distance, xlim=c(0,20),xlab="Distance to Nearest Protected Area (km)",ylab="Reported Conflict Point (Bear = 1, Other = 0)")
B1 <- glm(bears_presence~b2pa.distance,family="binomial")

# Creating a quadratic term for the model (since data is bell shaped)
B1.sq <- glm(bears.reproj$bears~b2pa.distance + I(b2pa.distance^2),family="binomial")
coef(B1.sq)
summary(B1.sq)
plot(B1.sq)
curve(plogis(-5.770904e-01+-1.740486e-05 *x),add=T,col="blue")

# Results indicate that there is an intercept of -6.4327 and slope of 0.000000105

# Trying this with bears only data:
just.bears<-read_csv("Data/just_bears_w_distance_km.csv")
jb.dist<-just.bears$distance_to_PAs
jb.pres<-just.bears$bears
plot(jitter(jb.pres)~jb.dist, xlim=c(0,18),xlab="Distance to Nearest Protected Area (km)",ylab="Reported Grizzly & Black Bear Conflict Points")

# Log Plot of Distances
log.dist<-log(jb.dist)
plot(jitter(jb.pres)~log.dist,xlab="Log of Distance to Nearest Protected Area (km)",ylab="Reported Grizzly & Black Bear Conflict Points")

p.j=plogis(Intercept+Slope*jb.dist)
jb.lik=dbinom(x=jb.pres,size = 1,prob = p.j,log = T)
jb.lik_sum=sum(jb.lik)
print(jb.lik_sum)
JB1 <- glm(jb.pres~jb.dist,family="binomial")
coef(JB1)
curve(plogis(2.656606e+01+ -4.091757e-16*x),add=T,col="blue")


# Random Effects by Local -------------------------------------------------
# This is the very beginning of messing with adding a random effect term. Need the
# proper CAN spatial data first
library(rstanarm)

b2pa.distance<-bears.full.join.$distance_to_PAs
bears_presence<-bears.reproj$bears

B1.mix <- stan_glmer(bears_presence~b2pa.distance+(1|CFSAUID),family="binomial", data=bears.full.join.)
plot(B1.mix)
plot(B1.mix, par="CFSAUID")
ranef(B1.mix) # The smallest random effect intercept is V8S at -2.9006
# The largest random effect intercept is V5A at 3.5709
head(ranef(B1.mix))


# Making Stack Overflow Ex ------------------------------------------------
dput(head(bears.reproj[1:3]))
dput(head(census.reproj[1:3,c(1,5,9)]))

