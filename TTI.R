rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "tidyverse", "sp", "raster", "here")
pack(packages)
# Load data
ttOrig <- st_read(here("input", "combined_tti_wtrolley.shp")) # 24,395 obs
# Compute priority score
ttShp <- ttOrig %>%
  dplyr::select(TTI) %>%
  filter(TTI != -1) %>%                                       # 21,606 obs
  filter(TTI != 0) %>%                                        # 21,149 obs
  mutate(lnTTI = log(TTI),
         ttScore = pnorm(lnTTI,
                         mean = mean(lnTTI),
                         sd = sd(lnTTI)) * 100)
# Rasterize
ttScore <- as(ttShp, "Spatial")
blank <- raster(ext = extent(ttScore),
                res = c(50, 50),
                crs = CRS("+init=epsg:26918"))
tt50 <- rasterize(ttScore, blank, field = ttScore@data[,3], fun = mean)
tt50 <- calc(tt50, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(tt50) <- CRS("+init=epsg:26918")
writeRaster(tt50, here("output", "tti.tif"), overwrite = TRUE)
