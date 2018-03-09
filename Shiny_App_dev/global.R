library(ggmap)

load("data/fatalities.rda")
tennessee <- c(lon = -86.5804, lat = 35.5175)
TN_map <- get_map(location = tennessee, zoom = 7,scale = 2, maptype = "roadmap")