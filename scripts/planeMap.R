
# planeMap
library(dplyr)
library(ggplot2)

flights <- readRDS("data/assembled_flights.rds")
# flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")
# names(flights) <- sub(" ", "", names(flights))

xl <- range(flights$Longitude)
yl <- range(flights$Latitude)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())
theme_empty <- theme(axis.text = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = "none")

nUnique <- function(x) length(unique(x))
apply(flights, 2, nUnique)

left <- filter(flights, Longitude < 0 & Longitude > -180)
right <- filter(flights, Longitude > 0 & Longitude < 180)

pdf("plots/planeMap.pdf", 8, 4)
ggplot(flights, aes(x = Longitude, y = Latitude, 
                    group = Callsign, colour = Altitude)) +
  geom_path(size = 0.1, alpha = 0.05, data = left) +
  geom_path(size = 0.1, alpha = 0.05, data = right) +
  coord_map() +
  theme_empty +
  theme_stripped
dev.off()