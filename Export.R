# BEFORE RUNNING THIS SCRIPT
# RASTER CALCULATIONS MUST BE RUN IN QGIS
# SEE `Raster Resample Formulas.txt`

rm(list=ls())
library(raster); library(rgdal); library(BAMMtools); library(here)

weightedScr <- raster(here("process", "weightedScore.tif"))
weightedScr <- reclassify(weightedScr, cbind(-Inf, 0, NA), right = TRUE)
# First, use focal mean to smooth result. fc = "focal"
fcWeightedScr <- focal(weightedScr,
                       w = matrix(1, nrow = 3, ncol = 3),
                       fun = mean, pad = TRUE, na.rm = TRUE)
fcWeightedScr <- calc(fcWeightedScr, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
# Next, cut this file down to another raster extent. Try addRast.tif:
exists <- raster(here("process", "addRast.tif"))
class0 <- c(0, 5, 1,
            5,Inf,0,
            -Inf,0,0)
class0 <- matrix(class0, ncol = 3, byrow = TRUE)
reclass0 <- reclassify(exists, class0)
# Multiply this file by smWeightedScr to get a smoothed file for weightedScore.tif. Done in QGIS
crs(reclass0) <- CRS("+init=epsg:26918")
crs(fcWeightedScr) <- CRS("+init=epsg:26918")
writeRaster(reclass0, here("process", "ws_multiplier.tif"), overwrite = TRUE)
writeRaster(fcWeightedScr, here("process", "ws_focal.tif"), overwrite = TRUE)
# Import raster calc result from QGIS. sm = "smoothed"
smWeightedScr <- raster(here("process", "ws_smoothed.tif"))
# Stretch range from 0-80.2 to 0-100. str = "stretched"
smWeightedScr <- calc(smWeightedScr, fun = function(x){ x[x == 0] <- NA; return(x)} )
storedMax <- cellStats(smWeightedScr, stat = "max", na.rm = TRUE)
strWeightedScr <- smWeightedScr / storedMax * 100

# Now, let's do the same for the raster that overlays ridership and overallScore.
# Multiplied ws_focal * riderOverall in QGIS and exported as riderOverlay.tif. sm = "smoothed"
smRiderScr <- raster(here("process", "riderOverlay.tif"))
# Stretch range from 0-69.4 to 0-100. str = "stretched"
smRiderScr <- calc(smRiderScr, fun = function(x){ x[x == 0] <- NA; return(x)} )
storedmax <- cellStats(smRiderScr, stat = "max", na.rm = TRUE)
strRiderScr <- smRiderScr / storedmax * 100

# FINAL EXPORTS
# Continuous raster
writeRaster(strWeightedScr, here("output", "priority_score.tif"), overwrite = TRUE)
writeRaster(strRiderScr, here("output", "priority_score_incl_ridership.tif"), overwrite = TRUE)

# Shapefiles instead of rasters
# dist = "distribution"
# rec = "reclassified"
strWeightedVec <- na.omit(as.vector(as.matrix(strWeightedScr)))
strRiderVec <- na.omit(as.vector(as.matrix(strRiderScr)))
recWeightedScr <- cut(strWeightedScr,
                      breaks = getJenksBreaks(strWeightedVec, 21))
distRiderScr <- strRiderScr@data@values
distRiderScr <- na.omit(distRiderScr)
recRiderScr <- cut(strRiderScr,
                   breaks = getJenksBreaks(strRiderVec, 21))
polyWeightedScr <- rasterToPolygons(recWeightedScr, na.rm = TRUE, dissolve = FALSE)
polyRiderScr <- rasterToPolygons(recRiderScr, na.rm = TRUE, dissolve = FALSE)
# Reproject to EPSG 4326
polyWeightedScr <- spTransform(polyWeightedScr, CRS("+init=epsg:4326"))
polyRiderScr <- spTransform(polyRiderScr, CRS("+init=epsg:4326"))
writeOGR(polyWeightedScr, here("output"), "class20Score_jk", driver = "ESRI Shapefile")
writeOGR(polyRiderScr, here("output"), "class20Rider_jk", driver = "ESRI Shapefile")
