
# 43 Chilton Rd
# Richmond TW9 4JD, UK
# planeMap
library(dplyr)
library(ggplot2)

flights <- readRDS("data/assembled_flights.rds")

eu <- readRDS("data/euShape.rds")
names(eu)[1:2] <- c("Longitude", "Latitude")

### FUNCTIONS
flightPaths <- function(region, underlay, lines, Lat, Lon, point) {
  linesSub <- filter(lines, Latitude >  Lat[1] & Latitude <  Lat[2] & Longitude > Lon[1] & Longitude < Lon[2])
  mapSub <- filter(underlay, Latitude >  Lat[1] & Latitude <  Lat[2] & Longitude > Lon[1] & Longitude < Lon[2])
  theme_stripped <- theme(panel.background = element_blank(), panel.grid = element_blank())
  theme_empty <- theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "none")
  xl <- range(linesSub$Longitude) ;  yl <- range(linesSub$Latitude)
  p1 <- ggplot(linesSub, aes(x = Longitude, y = Latitude, group = Callsign, colour = Altitude)) +
    geom_path(size = 0.1, alpha = 0.1) + 
    geom_point(aes(x = point[1], y = point[2]), colour = "#ff000050") +
    coord_map() + theme_empty + theme_stripped
  print(p1)
  
  p2 <- ggplot(linesSub, aes(x = Longitude, y = Latitude)) +
    geom_polygon(aes(group = group), mapSub, fill = grey(0.85)) +
    geom_path(aes(group = Callsign, colour = Altitude), size = 0.1, alpha = 0.05) +
    coord_map() +
    geom_text(label = region, x = Lon[1], y = Lat[2], 
              colour = "navyblue", size = 5, hjust = 0, vjust = 1) +
    geom_point(aes(y = point[1], x = point[2]), colour = "#ff000050") +
    theme_empty + theme_stripped
  print(p2)
}
calcHeight <- function(regionLon, regionLat, width) {
  width*(diff(regionLat)/diff(regionLon))
}

width <- 500

# UK
name <- "Thom&Ibby"
map <- eu
Lat <- c(50, 58) 
Lon <- c(-10, 2)
point <- c(51.469212, -0.284316)
png(paste0("plots/", name, "%03d.png"), 
    width, calcHeight(Lat, Lon, width), "px")
flightPaths(name, map, flights, Lat, Lon, point)
dev.off()

