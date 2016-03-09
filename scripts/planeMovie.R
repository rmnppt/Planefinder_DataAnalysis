
# planeMap
library(dplyr)
library(lubridate)
library(ggplot2)
library(animation)

flights <- readRDS("data/assembled_flights.rds")
flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")

oneMin <- duration(1, "minutes")
start <- as.POSIXct("2014-07-01 00:00:00 BST")
end <- as.POSIXct("2014-07-02 00:00:00 BST" )
end-start
times <- seq(start, end, by = oneMin)

xl <- range(flights$Longitude)
yl <- range(flights$Latitude)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())
theme_empty <- theme(axis.text = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks = element_blank())

theseMins <- 1:(length(times)-20)
theseMinsThin <- theseMins[which(theseMins %% 10 == 0)]
length(theseMins) / length(theseMinsThin)

subs <- list()
for(i in theseMinsThin){
  subs[[i]] <- flights %>% 
    filter(time > times[i] & time < times[i+20]) %>%
    mutate(ID = paste(Registration, Callsign, ADSHEX))
  cat("_", i, "_")
}

flightsWireFrame <- flights %>% slice(1)
baseplot <- ggplot(flightsWireFrame, aes(x = Longitude, y = Latitude)) +
  coord_map() +
  ylim(yl) +
  xlim(xl) +
  theme_stripped +
  theme_empty

saveGIF({
  for(i in theseMinsThin) { 
    p <- baseplot + geom_path(data = subs[[i]], aes(group = ID),
                              alpha = 0.5, size = 0.5)
    print(p)
  }
}, 
interval = 0.01, movie.name = "planeMovie.gif", 
ani.width = 600, ani.height = 400)


