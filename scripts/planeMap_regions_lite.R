
# planeMap
library(dplyr)
library(ggplot2)

flights <- readRDS("data/assembled_flights.rds")

eu <- readRDS("data/euShape.rds")
names(eu)[1:2] <- c("Longitude", "Latitude")

world <- readRDS("data/worldShape.rds")
names(world)[1:2] <- c("Longitude", "Latitude")

usa <- readRDS("data/USAShape.rds")
names(usa)[1:2] <- c("Longitude", "Latitude")

# asia <- readRDS("data/asiaShape.rds")
# names(asia)[1:2] <- c("Longitude", "Latitude")

### FUNCTIONS
flightPaths <- function(region, underlay, lines, Lat, Lon) {
  linesSub <- filter(lines, Latitude >  Lat[1] & Latitude <  Lat[2] & Longitude > Lon[1] & Longitude < Lon[2])
  mapSub <- filter(underlay, Latitude >  Lat[1] & Latitude <  Lat[2] & Longitude > Lon[1] & Longitude < Lon[2])
  theme_stripped <- theme(panel.background = element_blank(), panel.grid = element_blank())
  theme_empty <- theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "none")
  xl <- range(linesSub$Longitude) ;  yl <- range(linesSub$Latitude)
  p1 <- ggplot(linesSub, aes(x = Longitude, y = Latitude, group = Callsign, colour = Altitude)) +
    geom_path(size = 0.1, alpha = 0.2) + coord_map() + theme_empty + theme_stripped
  print(p1)
  p2 <- ggplot(linesSub, aes(x = Longitude, y = Latitude)) +
    geom_polygon(aes(group = group), mapSub, fill = grey(0.85)) +
    geom_path(aes(group = Callsign, colour = Altitude), size = 0.1, alpha = 0.2) +
    coord_map() +
#     geom_text(label = region, x = Lon[1], y = Lat[2], 
#               colour = "navyblue", size = 5, hjust = 0, vjust = 1) +
    theme_empty + theme_stripped
  print(p2)
}
calcHeight <- function(regionLon, regionLat, width) {
  width*(diff(regionLat)/diff(regionLon))
}
###

### This suffers the left/right problem
# # World
# name <- "World"
# map <- world
# Lat <- c(-89.9, 89.9)
# Lon <- c(-179.9, 179.9)
# pdf(paste0("plots/", name, ".pdf"), 5, calcHeight(Lat, Lon), onefile = TRUE)
# flightPaths(name, map, flights, Lat, Lon)
# dev.off()

width <- 1000

# UK
name <- "UK"
map <- eu
Lat <- c(50, 58) 
Lon <- c(-10, 2)
png(paste0("plots/", name, "%03d.png"), 
    width, calcHeight(Lat, Lon, width), "px")
flightPaths(name, map, flights, Lat, Lon)
dev.off()

# Japan
name <- "Japan"
map <- world
Lat <- c(30, 40)
Lon <- c(131, 143)
png(paste0("plots/", name, "%03d.png"), 
    width, calcHeight(Lat, Lon, width), "px")
flightPaths(name, map, flights, Lat, Lon)
dev.off()

# New York
name <- "New York"
map <- usa
Lat <- c(39, 42)
Lon <- c(-75, -70)
png(paste0("plots/", name, "%03d.png"), 
    width, calcHeight(Lat, Lon, width), "px")
flightPaths(name, map, flights, Lat, Lon)
dev.off()

# East Australia
name <- "East Australia"
map <- world
Lat <- c(-40, -13)
Lon <- c(135, 160)
png(paste0("plots/", name, "%03d.png"), 
    width, calcHeight(Lat, Lon, width), "px")
flightPaths(name, map, flights, Lat, Lon)
dev.off()

# India
name <- "India"
map <- world
Lat <- c(7, 33)
Lon <- c(67, 90)
png(paste0("plots/", name, "%03d.png"), 
    width, calcHeight(Lat, Lon, width), "px")
flightPaths(name, map, flights, Lat, Lon)
dev.off()




