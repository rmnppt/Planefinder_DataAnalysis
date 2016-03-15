################################################################################
## Measuring delays in flights
## Adarsh Janakiraman (Jan 2016)
################################################################################
library(dplyr)

theme_stripped <- theme(panel.background = element_blank(),
                        panel.grid = element_blank())

colNames  <- c("AirportID", "AirportName", "City","Country","Code", "ICAO", "Latitude", "Longitude", "Altitude", "Timezone", "DST", "TzName")
airports   <- read.csv("data/airports.dat", header = FALSE, col.names=colNames)

colNames <- c("AirlineID", "AirlineName","Alias","IATA","AirlineICAO", "Callsign","AirlineCountry","Active")
airlines <- read.csv("data/airlines.dat", header=FALSE, col.names = colNames)


flights <- readRDS("data/assembled_flights.rds")
flights$time <- as.POSIXct(flights$mtime, origin = "1969-12-31 23:00:10")
names(flights) <- sub(" ", "", names(flights))
flights$destination  <- substr(flights$Route,5,7)
flights$origin <- substr(flights$Route,1,3)
flights$IATA <- substr(flights$FlightNumber, 1,2)

#joining flights and airports data
flightsAndAiports <- left_join(flights, airports, by =c("origin" = "Code"))

#join it with the Airline data too
flightsAndAiports <- left_join(flightsAndAiports, airlines, by =c("IATA" = "IATA"))

#filtering for UK only flights (using geometrics)
#flightsUK <- filter(flights, Latitude  < 58 & Latitude  > 50 &
#                      Longitude < 2  & Longitude > -10)

saveRDS(flightsAndAiports, "data/flightsAndAirports.rds")

#filtering by Country code after airports join (seems more accurate. smaller db)
flightsUK <- filter(flightsAndAiports, Country == 'United Kingdom')


altThreshold1 <- 100
altThreshold2 <- 5
# need to normalise the times
# and include only flights which are taking off (alt[1] <100)
takeOff <- flightsUK %>%
  group_by(FlightNumber) %>%
  mutate(ntime = mtime - min(mtime)) %>%
  filter(Altitude.x[1] < altThreshold1 & ntime < 4000)

FlightNumbers <- unique(takeOff$FlightNumber)
takeOff %>%
  filter(FlightNumber == FlightNumbers[4]) %>%
  select(matches("time"), Altitude.x) %>%
  head


### make altitude condition to be greater than first value.
delayInfo <- takeOff %>% 
  filter(Altitude.x > 0) %>% 
  filter(ntime==min(ntime)) %>% 
  select(ntime, Altitude.x, time, Country, City, AirportName, Type, AirlineName, AirlineCountry) %>%
  droplevels

saveRDS(delayInfo, "data/UkDelayTimes.rds")


