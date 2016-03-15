library(dplyr)
library(ggplot2)

d <- readRDS("data/flightsAndAirports.rds")
N <- function(x) length(unique(x))

UK <- d %>% filter(Country == "United Kingdom")

airports <- UK %>%
  group_by(AirportName) %>%
  summarise(count = N(FlightNumber)) %>%
  arrange(desc(count)) %>%
  mutate(AirportName = paste0(1:nrow(.), ". ", AirportName)) %>%
  slice(c(1:10, 25:29))

pdf("delay_analysis/airportCounts.pdf", 6, 4)
ggplot(airports, aes(x = reorder(AirportName, count), y = count)) +
  geom_point(colour = "red", size = 2) +
  geom_text(aes(label = count, x = AirportName, y = count + 15), hjust = 0) +
  geom_segment(aes(x = AirportName, xend = AirportName, 
                   y = count, yend = 0),
               colour = "red") +
  coord_flip() +
  xlab("") +
  ylab("") +
  ylim(0, 750) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0, size = 10))
dev.off()

