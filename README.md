## ONA-GRIZZ-shannon
Below are descriptions of each code script and where to obtain the data for each of them. --------------------------------------------------------

The data for all of the following scripts can be found at this googledrive link: https://drive.google.com/drive/u/0/folders/1Sp6uUMJ0hme1vn_popIcS6mJN5Ai5Z4p

The below scripts include the process we followed for collecting and cleaning our data, building a "master data frame", and running our data through a linear regression. The order of the scripts that reproduces that data frame is as follows:


############# WARP Dominant Farm Type.R ###################
This script imports agriculture census data from Canada, filters it to just British Columbia (B.C.), and converts it to a spatial data frame by joining the tabular agriculture data with the spatial census consolidated subdivisions (CCS) for B.C. This script loads in our farm type variable, where we can subset the resulting data frames to include a "dominant" farm type by CCS region and a total farm count by CCS region.

The data for this script can be found at this google drive link, in the Census of AGriculture - BC / Farm Type folder: 
https://drive.google.com/drive/u/0/folders/1xDoTkb78w-IcGSNDtT5iRAetx4CoeaTp


################# WARP All Species Master Prep.R #######################
This script includes the first part of producing our "master data frame" by bringing in all species WARP reports for our time frame, creating columns for our variables (dist to PA's, dist to Metro, Farm Type, and Farm count) for the all-species full year data and then compiling these into an almost complete "master data frame".

The data for this script can be found at this google drive link, in the WARP All Species folder:
https://drive.google.com/drive/u/0/folders/1juDh_WCrWV9PMOio9PXyoTscyGQjk7h3


################ Producing GrizzIncrease Raster.R #######################
This script is where we bring in the sociobio and combined resistance rasters and preform raster math to create our grizzincrease raster (which represents the proportion of residents that support increases in grizz populations). Additionally, we bring in the grizz density and resample it to match our other raster extents and resolutions.

The data for this script can be found at this google drive link under the folder Biophs & Grizzinc Current Maps / Code Script Data Files:
https://drive.google.com/drive/u/0/folders/1nK1mK37W2ahWTx6ZPFvfehFBe8OMX9FH


############ Attributing CS Outputs to WARP Points.R ####################
In this script we be bring in the produced "master data frame" resulting from our All Species Master Prep script as a sf data frame and our connectivity surfaces (CS's) for the grizzincrease CS, biophysical CS, grizzly density (bear habitat suitability or BHS) CS, and survey resistance surface as rasters. We overlay each individual raster with the WARP points, buffer the points by 500m and conv ert them to a Spatvector, and then extract the attributes from each raster to each WARP point by location. The result is four additional columns (one for each raster) in the master WARP data frame, representing these values. Lastly, we check for any NA's present within our variable columns, and drop those rows from the data frame. This produces our final "master data frame" which can now be used in our regression script.

The data for the produced CS's exists in the googledrive link under the folder Biophs & Grizzinc Current Maps / Code Script Data Files:
https://drive.google.com/drive/u/0/folders/1nK1mK37W2ahWTx6ZPFvfehFBe8OMX9FH


############## WARP Conflict Binomial Regression.R #####################
This script is where we bring in our master data frame and run a series of liner regressions with individual and combined covariates from the variable columns that we produced above. Here we run individual and full models, run model comparisons, and observe interactions terms.

The master data frame for this script can be found at this google drive link under the folder WARP Master DF (+CS resistance values):
https://drive.google.com/drive/u/0/folders/1gU-vTsoBNsUVQS0H39suUldusNDJgfw3



