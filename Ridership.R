rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "rgdal", "maptools",
              "raster", "BAMMtools", "here")
pack(packages)

# 1. COMPUTE PRIORITY SCORES FOR ALL TRANSIT LINES
# Will become priority scores for NJ side
lineShp <- readOGR(here("input"), "statsbyline_allgeom")
line <- as.data.frame(lineShp)
# Remove 0 ridership
line$dailyrider[line$dailyrider == 0] <- NA; line <- line[!is.na(line$dailyrider),]
line <- line[c("linename", "name", "dailyrider")]
line$ridershipScore <- pnorm(line$dailyrider,
                             mean = mean(line$dailyrider),
                             sd = sd(line$dailyrider),
                             lower.tail = TRUE) * 100
line <- line[c("linename", "ridershipScore")]
lineShp <- merge(lineShp, line, by = "linename")
riderScore <- spTransform(lineShp, CRS("+init=epsg:26918"))
blank <- raster(ext = extent(374610.3, 586310.3, 4309335, 4538285),
                res = c(50, 50),
                crs = CRS("+init=epsg:26918"))
rider50 <- rasterize(riderScore, blank, field = riderScore@data[,11], fun = mean)
rider50 <- calc(rider50, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(rider50) <- CRS("+init=epsg:26918")
writeRaster(rider50, here("process", "rider50.tif"), overwrite = TRUE)

# 2. COMPUTE TOTAL RIDERSHIP, SEPTA BUS AND TROLLEY LOADS
bus <- readOGR(here("input"), "Surface_Transit_Loads")
bus <- spTransform(bus, CRS("+init=epsg:26918"))
# Oi. On this one we'll have to rasterize first and assign priority score later
rider50 <- rasterize(bus, blank, field = bus@data[,5], fun = sum)
# Some ridership records exceed 300,000: max is 325,219.9.
# If ridership > 95th percentile, recode to 95th percentile
pct95 <- quantile(na.omit(rider50@data@values), c(0.95))
rider50 <- calc(rider50, fun = function(x){ x <- ifelse(x > pct95, pct95, x); return(x)} )
# Assign new priority score
rider50 <- calc(rider50, fun = function(x){ x <- pnorm(x, mean(x, na.rm = TRUE),
                                                       sd(x, na.rm = TRUE),
                                                       lower.tail = TRUE) * 100; return(x)} )
rider50 <- calc(rider50, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(rider50) <- CRS("+init=epsg:26918")
writeRaster(rider50, here("process", "rider50pa.tif"), overwrite = TRUE)

# 3. RASTERIZE NJ AND PA LINES SEPARATELY
# Allows for comparison between NJ line data and PA link data
# Must also preserve full extent of ridership file
rider <- readOGR(here("input"), "statsbyline_allgeom")
rider$type <- as.character(substr(rider$name, 1, 3))
riderPA <- subset(rider, type == "sep")
riderNJ <- subset(rider, type == "njt")
riderPA <- spTransform(riderPA, CRS("+init=epsg:26918"))
riderPA$gid <- 100
rider50PA <- rasterize(riderPA, blank, field = riderPA@data[,1])
rider50PA <- calc(rider50PA, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(rider50PA) <- CRS("+init=epsg:26918")
writeRaster(rider50PA, here("process", "riderPA.tif"), overwrite = TRUE)

riderNJ <- spTransform(riderNJ, CRS("+init=epsg:26918"))
riderNJ$gid <- 100
rider50NJ <- rasterize(riderNJ, blank, field = riderNJ@data[,1])
rider50NJ <- calc(rider50NJ, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(rider50NJ) <- CRS("+init=epsg:26918")
writeRaster(rider50NJ, here("process", "riderNJ.tif"), overwrite = TRUE)
