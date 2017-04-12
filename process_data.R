library(dplyr)
library(stplanr)
# Read data
dat <- read.csv("GpsTrips.csv", header = T, as.is = T, stringsAsFactors = F)
# Subset cycling trips
dat_bike <- filter(dat, travelmode == 2)
# Subset walking trips
dat_walk <- filter(dat, travelmode == 1)

# Test route
for (i in 1:nrow(dat_bike)){
  start_point <- c(dat_bike$startlong[i], dat_bike$startlat[i])
  end_point <- c(dat_bike$endlong[i], dat_bike$endlat[i])
  cat(i, " - ", start_point, " - ", end_point, "\n")
  if ( !i %in% c(42, 50, 66)){#!identical(start_point, end_point))
    rdata <- route_graphhopper(start_point, end_point, vehicle = "bike")
    # "time"        "dist"        "change_elev"
    dat_bike$time[i] <- rdata$time
    dat_bike$dist[i] <- rdata$dist
    dat_bike$change_elev[i] <- rdata$change_elev
  }
}
# 


