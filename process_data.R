library(dplyr)
library(stplanr)
# Read updated_route_graphhopper function
source("route_functions.R")
# Read data
dat <- read.csv("GpsTrips.csv", header = T, as.is = T, stringsAsFactors = F)

# Treat all trips as bike trips

for (i in 1:nrow(dat)){
  # Create start point
  start_point <- c(dat$startlong[i], dat$startlat[i])
  # Create end point
  end_point <- c(dat$endlong[i], dat$endlat[i])
  rdata <- updated_route_graphhopper(start_point, end_point, vehicle = "bike")
  # If a trip data is not null, get the variable info
  if ( !is.null(rdata)){
    dat$time[i] <- rdata$time
    dat$dist[i] <- rdata$dist
    dat$descend[i] <- rdata$descend
    dat$ascend[i] <- rdata$ascend
  }else { # Else treat all variables as zero
    dat$time[i] <- 0
    dat$dist[i] <- 0
    dat$descend[i] <- 0
    dat$ascend[i] <- 0
  }
}

