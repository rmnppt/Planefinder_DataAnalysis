
# planeMap
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(animation)

flights <- readRDS("data/assembled_flights.rds")
Lat <- c(50, 58) 
Lon <- c(-10, 2)
flights <- flights %>% 
  filter(Longitude > Lon[1] & Longitude < Lon[2] &
         Latitude > Lat[1] & Latitude < Lat[2])

flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")

oneMin <- duration(1, "minutes")
start <- as.POSIXct("2014-07-01 00:00:00 BST")
end <- as.POSIXct("2014-07-02 00:00:00 BST" )
end-start
times <- seq(start, end, by = oneMin)

xl <- range(flights$Longitude)
yl <- range(flights$Latitude)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank(),
                        title = element_text(hjust = 0, 
                                             colour = grey(0.5)),
                        legend.position = "none")
theme_empty <- theme(axis.text = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank())

window <- 20

theseMins <- 1:(length(times)-(2*window))
theseMins <- theseMins[which(theseMins %% window == 0)]
length(theseMins) / length(theseMinsThin)

subs <- list()
for(i in theseMins){
  subs[[i]] <- flights %>% 
    filter(time > times[i] & time < times[i+(2*window)]) %>%
    mutate(ID = paste(Registration, Callsign, ADSHEX))
  cat("_", i, "_")
}

flightsWireFrame <- flights %>% slice(1)
baseplot <- ggplot(flightsWireFrame, 
                   aes(x = Longitude, y = Latitude)) +
  coord_map() +
  ylim(yl) +
  xlim(xl) +
  theme_stripped +
  theme_empty

altitudePlot <- function(dat) {
  p <- ggplot(flights, aes(x = Altitude, y = Type, colour = Type)) +
    geom_point(data = dat) +
    theme_stripped
}

saveGIF({
  for(i in theseMins) { 
    p1 <- baseplot + 
      geom_path(data = subs[[i]], 
                aes(group = ID, alpha = mtime, colour = Type),
                size = 0.5) +
      ggtitle(paste(hour(max(subs[[i]]$time)), ":00"))
    top <- filter(subs[[i]], time == max(time))
    p2 <- altitudePlot(top)
    grid.arrange(p1, p2, nrow = 1, widths = c(5, 2), heights = 5)
  }
}, 
interval = 0.1, movie.name = "planeMovie_UK_multipanel.gif", 
ani.width = 400, ani.height = 600)


