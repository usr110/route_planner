library(dplyr)
library(stplanr)
# Read data
dat <- read.csv("GpsTrips.csv", header = T, as.is = T, stringsAsFactors = F)
# Subset cycling trips
dat_bike <- filter(dat, travelmode == 2)
# Subset walking trips
dat_walk <- filter(dat, travelmode == 1)
# Test route
start_point <- c(dat_bike$startlong[1], dat_bike$startlat[1])
end_point <- c(dat_bike$endlong[1], dat_bike$endlat[1])
rdata <- route_graphhopper(start_point, end_point, vehicle = "bike")


