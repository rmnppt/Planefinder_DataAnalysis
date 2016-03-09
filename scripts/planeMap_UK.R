
# planeMap
library(dplyr)
library(ggplot2)

flights <- readRDS("data/assembled_flights.rds")
# flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")
# names(flights) <- sub(" ", "", names(flights))

flightsUK <- filter(flights, Latitude < 58 & Latitude > 50 &
                             Longitude < 2 & Longitude > -10)

xl <- range(flightsUK$Longitude)
yl <- range(flightsUK$Latitude)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())
theme_empty <- theme(axis.text = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = "none")

# nUnique <- function(x) length(unique(x))
# apply(flightsUK, 2, nUnique)

pdf("plots/planeMap_UK.pdf", 6, 7)
ggplot(flightsUK, aes(x = Longitude, y = Latitude, 
                    group = Callsign, colour = Altitude)) +
  geom_path(size = 0.1, alpha = 0.05) +
  coord_map() +
  theme_empty +
  theme_stripped
dev.off()