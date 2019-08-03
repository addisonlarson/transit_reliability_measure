rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "rgdal", "maptools", "raster", "tidyverse", "here")
pack(packages)

linkspeedShp <- readOGR(here("input"), "LinkSpeed_byLine")
linkspeed <- as.data.frame(linkspeedShp)
# Empty and 0 values replaced with NA
linkspeed[linkspeed == 0] <- NA; linkspeed <- na.omit(linkspeed)
# Separate out by what type of transit each link is
codes <- read_csv(here("input", "linkspeedBylineNameCode.csv"))
linkspeed <- merge(linkspeed, codes, by = "linename")
# Keep Bus, Trl
linkspeed <- subset(linkspeed, tsyscode == "Bus" | tsyscode == "Trl")
# Split by identical from-to pairs
linkspeed$fromToPaste <- paste(linkspeed$fromnodeno, linkspeed$tonodeno, sep = "_")
linkspeedShp$fromToPaste <- paste(linkspeedShp$fromnodeno, linkspeedShp$tonodeno, sep = "_")
uniqueLinks <- linkspeed[9][!duplicated(linkspeed[9]),]
# Weighted mean of average speed for each link
# Weight is the cnt, the number of times the route crosses the link
indivLinkSpeed <- data.frame()
for (i in uniqueLinks){
  obs <- linkspeed[which(linkspeed$fromToPaste == i),]
  wavg <- weighted.mean(obs$avgspeed, obs$cnt)
  myrow <- data.frame(fromToPaste = i, avgSpdW = wavg)
  indivLinkSpeed <- rbind(indivLinkSpeed, myrow)
}

indivLinkSpeed$avgSpdW <- ifelse(indivLinkSpeed$avgSpdW > 75, 75, indivLinkSpeed$avgSpdW)

indivLinkSpeed$lnAvgSpdW <- log(indivLinkSpeed$avgSpdW)
indivLinkSpeed$linkspeedScore <- pnorm(indivLinkSpeed$lnAvgSpdW,
                                       mean = mean(indivLinkSpeed$lnAvgSpdW),
                                       sd = sd(indivLinkSpeed$lnAvgSpdW),
                                       lower.tail = FALSE) * 100
indivLinkSpeed <- indivLinkSpeed[c(1,4)]
linkspeedShp <- merge(linkspeedShp, indivLinkSpeed, by = "fromToPaste")
linkspeedShp <- linkspeedShp[c(1,9)]
linkspeedShp <- linkspeedShp[!duplicated(data.frame(linkspeedShp)),]

linkspeedScore <- spTransform(linkspeedShp, CRS("+init=epsg:26918"))
blank <- raster(ext = extent(374610.3, 586310.3, 4309335, 4538285),
                res = c(50, 50),
                crs = CRS("+init=epsg:26918"))
linkspeed50 <- rasterize(linkspeedScore, blank, field = linkspeedScore@data[,2], fun = mean)
linkspeed50 <- calc(linkspeed50, fun = function(x){ x[is.na(x)] <- 0; return(x)} )
crs(linkspeed50) <- CRS("+init=epsg:26918")
writeRaster(linkspeed50, here("output", "linkspeed50.tif"), overwrite = TRUE)
