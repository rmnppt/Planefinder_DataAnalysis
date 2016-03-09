# landing and takeoff
# planeMap
library(dplyr)
library(ggplot2)
library(lubridate)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())

flights <- readRDS("data/assembled_flights.rds")
flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")
names(flights) <- sub(" ", "", names(flights))

flightsUK <- filter(flights, Latitude  < 58 & Latitude  > 50 &
                             Longitude < 2  & Longitude > -10)

# need to normalise the times
takeOff <- flightsUK %>%
  group_by(FlightNumber) %>%
  mutate(ntime = mtime - min(mtime)) %>%
  filter(Altitude[1] < 100 & ntime < 4000)

# did it work?
Callsigns <- unique(takeOff$Callsign)
FlightNumbers <- unique(takeOff$FlightNumber)
Types <- unique(takeOff$Type)
takeOff %>%
  filter(FlightNumber == FlightNumbers[20]) %>%
  select(matches("time"), Altitude) %>%
  head

yl <- range(takeOff$Altitude)

for(i in 1:length(Types)) {
  p <- ggplot(filter(takeOff, Type == Types[i]), 
              aes(x = ntime, y = Altitude, 
                  group = FlightNumber, 
                  colour = Type)) +
    geom_line(alpha = 0.5) + 
    ylim(yl) +
    theme_stripped +
    scale_color_brewer(palette = "Set1")
  pdf(paste("plots/altTrajectories_UK", Types[i], ".pdf"), 6, 3)
  print(p)
  dev.off()
}




