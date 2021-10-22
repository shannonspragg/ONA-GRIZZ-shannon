# cropn to MT PAs:

library(sf)
library(tidyverse)
library(dplyr)

us.pas <- st_read("/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/Spatial Data in R/Mini Project Data/US PA's/PADUS2_0Proclamation.shp")

head(us.pas)


MT.PAs <- us.pas %>% 
  filter(., State_Nm == "MT") %>% 
  st_make_valid()

st_write(MT.PAs,"/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/Spatial Data in R/Mini Project Data/US PA's/MT_PAs.shp")
mt.pas.check <- st_read("/Users/shannonspragg/Desktop/Boise State/BSU Research Lab/R Tools/Spatial Data in R/Mini Project Data/US PA's/MT_PAs.shp")
 plot(st_geometry(mt.pas.check))
 