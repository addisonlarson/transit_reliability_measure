rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "rgdal", "maptools", "raster", "here")
pack(packages)

lineShp <- readOGR(here("input"), "statsbyline_allgeom")
line <- as.data.frame(lineShp)
line$otp[line$otp == 0] <- NA; line <- line[!is.na(line$otp),]
line$OTPscore <- pnorm(line$otp,
                       mean = mean(line$otp),
                       sd = sd(line$otp),
                       lower.tail = FALSE) * 100
# Lowest OTP lines get highest priority score
line <- line[c(2,11)]
lineShp <- merge(lineShp, line, by = "linename")

otpScore <- spTransform(lineShp, CRS("+init=epsg:26918"))
blank <- raster(ext = extent(374610.3, 586310.3, 4309335, 4538285),
                res = c(50, 50),
                crs = CRS("+init=epsg:26918"))
otp50 <- rasterize(otpScore, blank, field = otpScore@data[,11], fun = mean)
otp50 <- calc(otp50, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(otp50) <- CRS("+init=epsg:26918")
writeRaster(otp50, here("output", "otp50.tif"), overwrite = TRUE)
