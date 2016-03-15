library(dplyr)
library(ggplot2)

d <- readRDS("data/flightsAndAirports.rds")
N <- function(x) length(unique(x))

UK <- d %>% filter(Country == "United Kingdom")

TwoCities <- UK %>% 
  filter(AirportName == "Inverness" | AirportName == "Edinburgh")%>%
  mutate(id = paste(Registration, FlightNumber, ADSHEX, Callsign.x)) %>%
  group_by(id) %>%
  arrange(time) %>%
  filter(Altitude.x[1] < 20000) %>%
  mutate(ntime = mtime - min(mtime)) %>%
  droplevels 

TwoCities$AirportName <- factor(TwoCities$AirportName, c("Inverness", "Edinburgh"))

delay <- TwoCities %>% 
  filter(Altitude.x > Altitude.x[1]) %>% 
  filter(ntime==min(ntime))

pdf("delay_analysis/TwoCities.pdf", 7, 4, TRUE)
ggplot(TwoCities, 
       aes(x = ntime/60, y = Altitude.x)) +
  geom_line(aes(group = id), col = "red", alpha = 0.5) +
  xlim(0, 2000) +
  ylab("Altitude") +
  xlab("Imaginary Time") +
  facet_wrap(~ AirportName)

ggplot(TwoCities, 
       aes(x = ntime/60, y = Altitude.x)) +
  geom_line(aes(group = id), col = "red", alpha = 0.5) +
  geom_rug(data = delay, sides = "b", col = "red", alpha = 0.5) +
  xlim(0, 2000) +
  ylab("Altitude") +
  xlab("Imaginary Time") +
  facet_wrap(~ AirportName)
dev.off()
