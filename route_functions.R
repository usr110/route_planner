# Update stplan'r route_graphhopper function with updated_route_graphhopper
# This function changes in two aspects:
# 1. Returns descend and ascend values separately of a route
# 2. Returns these two variables for all sort of vehicle (including food)
updated_route_graphhopper <- function(from, to, vehicle = "bike", silent = TRUE, pat = NULL, base_url = "https://graphhopper.com"){
  
  # Convert character strings to lon/lat if needs be
  if(is.character(from) | is.character(to)){
    from <- rev(RgoogleMaps::getGeoCode(from))
    to <- rev(RgoogleMaps::getGeoCode(to))
  }
  
  if(is.null(pat))
    pat = api_pat("graphhopper")
  
  httrmsg = httr::modify_url(
    base_url,
    path = "/api/1/route",
    query = list(
      point = paste0(from[2:1], collapse = ","),
      point = paste0(to[2:1], collapse = ","),
      vehicle = vehicle,
      locale = "en-US",
      debug = 'true',
      points_encoded = 'false',
      key = pat
    )
  )
  if(silent == FALSE){
    print(paste0("The request sent was: ", httrmsg))
  }
  httrreq <- httr::GET(httrmsg)
  httr::stop_for_status(httrreq)
  
  if (grepl('application/json', httrreq$headers$`content-type`) == FALSE) {
    stop("Error: Graphhopper did not return a valid result")
  }
  
  txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")
  if (txt == "") {
    stop("Error: Graphhopper did not return a valid result")
  }
  
  obj <- jsonlite::fromJSON(txt)
  
  if (is.element("message", names(obj))) {
    if (grepl("Wrong credentials", obj$message) == TRUE) {
      stop("Invalid API key")
    }
  }
  route <- sp::SpatialLines(list(sp::Lines(list(sp::Line(obj$paths$points[[1]][[1]][,1:2])), ID = "1")))
  
  climb <- NA # to set elev variable up
  
  descend <- NA
  ascend <- NA
  
  # get elevation data for both bike and foot trips
  if(vehicle == "bike" || vehicle == "foot"){
    descend <- obj$path$descend
    ascend <- obj$paths$ascend
  }
  
  # Attribute data for the route
  df <- data.frame(
    time = obj$paths$time / (1000 * 60),
    dist = obj$paths$distance,
    descend = descend,
    ascend = ascend
  )
  
  route <- sp::SpatialLinesDataFrame(route, df)
  proj4string(route) <- CRS("+init=epsg:4326")
  route
  
}