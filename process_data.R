library(dplyr)
library(stplanr)
# Read data
dat <- read.csv("GpsTrips.csv", header = T, as.is = T, stringsAsFactors = F)
# Subset cycling trips
data_bike <- filter(dat, travelmode == 2)
# Subset walking trips
data_walk <- filter(dat, travelmode == 1)

# Routes for cycling trips
for (i in 1:nrow(data_bike)){
  start_point <- c(data_bike$startlong[i], data_bike$startlat[i])
  end_point <- c(data_bike$endlong[i], data_bike$endlat[i])
  cat(i, " - ", start_point, " - ", end_point, "\n")
  if ( !i %in% c(42, 50, 66)){#!identical(start_point, end_point))
    rdata <- route_graphhopper(start_point, end_point, vehicle = "bike")
    data_bike$time[i] <- rdata$time
    data_bike$dist[i] <- rdata$dist
    data_bike$change_elev[i] <- rdata$change_elev
  }else {
    
    data_bike$time[i] <- 0
    data_bike$dist[i] <- 0
    data_bike$change_elev[i] <- 0

  }
}


# Identify if the start and end points are not identical
#spoint <- ggmap::revgeocode(c(data_walk$startlong[i], data_walk$startlat[i]))
#epoint <- ggmap::revgeocode(c(data_walk$endlong[i], data_walk$endlat[i]))


# else{
#   cat(i, " - ", spoint, " - ", epoint, "\n")
# }    # "time"        "dist"        "change_elev"
#    data_walk$time[i] <- rdata$time
#    data_walk$dist[i] <- rdata$dist
#    data_walk$change_elev[i] <- rdata$change_elev
#  }


# Routes for walking trips
for (i in 1:nrow(data_walk)){
  start_point <- c(data_walk$startlong[i], data_walk$startlat[i])
  end_point <- c(data_walk$endlong[i], data_walk$endlat[i])
  dist <- geosphere::distm (start_point, end_point, fun = distHaversine)
  if ( !i %in% c(242, 337, 369, 373, 380, 381, 402, 448, 449, 490, 506, 611, 613, 614, 752, 753, 781, 782,
                 797, 829, 883, 886, 935, 936, 946, 948, 951, 963, 1049, 1059, 1062, 1085, 1115, 1151, 1152, 1153,
                 1155, 1156, 1174, 1322, 1381, 1382, 1387, 1417, 1435, 1468, 1493, 1522, 1524, 1534, 1547, 1619, 1634,
                 1637, 1638, 1710, 1714, 1783, 1837, 1935, 1936, 1937, 1938, 1940, 1941, 1942, 1944, 1958, 1960, 2086,
                 2128, 2251, 2320, 2323, 2451, 2522, 2529)){
    rdata <- updated_route_graphhopper(start_point, end_point, vehicle = "foot")
    data_walk$time[i] <- rdata$time
    data_walk$dist[i] <- rdata$dist
    # data_walk$change_elev[i] <- rdata$change_elev
    data_walk$descend[i] <- rdata$descend
    data_walk$ascend[i] <- rdata$ascend
    
    cat(i, " - ", dist , " - ", start_point, " - ", end_point, " - ", rdata$descend, " - ", rdata$ascend, "\n")
  }
  else {
    data_walk$time[i] <- 0
    data_walk$dist[i] <- 0
    data_walk$descend[i] <- 0
    data_walk$ascend[i] <- 0
    #data_walk$change_elev[i] <- 0
  }
}