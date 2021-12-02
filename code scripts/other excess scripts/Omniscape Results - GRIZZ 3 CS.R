# Omniscape Results - GRIZZ CS's ------------------------------------------
  # Here I am importing and plotting the results of running omniscape on the following:
      # 1 - Combined Resistance (all biophys, HII, and survey) connectivity surface (CS) outputs
      # 2 - Sociobio (all biophys + survey response) CS outputs
      # 3 - GrizzIncrease (Survey) (just social survey responses) CS outputs


# Combined Resistance CS Outputs ------------------------------------------
# Checking CS Results:
comb.resist.cum.curmap <- rast("/Users/shannonspragg/rasters/Combined Resistance_1/cum_currmap.tif")
plot(comb.resist.cum.curmap) #Here we have the cumulitive current map
comb.resist.flow.potential <- rast("/Users/shannonspragg/rasters/Combined Resistance_1/flow_potential.tif")
plot(comb.resist.flow.potential) # Here we have the flow potential map


# Sociobio CS Outputs -----------------------------------------------------
sociobio.cum.curmap <- rast("/Users/shannonspragg/rasters/SocioBio Resistance CS/cum_currmap.tif")
plot(sociobio.cum.curmap)
sociobio.flow.potential <- rast("/Users/shannonspragg/rasters/SocioBio Resistance CS/flow_potential.tif")
plot(sociobio.flow.potential)



# GrizzIncrease (Survey) CS Outputs ---------------------------------------
grizzinc.cum.curmap <- rast("/Users/shannonspragg/rasters/Social GrizzIncrease CS/cum_currmap.tif")
plot(grizzinc.cum.curmap)
grizzinc.flow.potential <- rast("/Users/shannonspragg/rasters/Social GrizzIncrease CS/flow_potential.tif")
plot(grizzinc.flow.potential)

sociobio.cum.curmap
grizzinc.cum.curmap

plot(sociobio.cum.curmap)
plot(grizzinc.cum.curmap)

