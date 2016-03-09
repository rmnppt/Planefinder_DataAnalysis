# scrape the airlines from wikipedia
library(rvest)
airline_codes <- html("https://en.wikipedia.org/wiki/List_of_airline_codes")
airlines <- airline_codes %>%
  html_nodes("td") %>%
  html_text()
head(airlines, 30)

every <- function(x, n) {
  s <- seq(1, nrow(x), by = n)
  sapply(x, function(x) x[s])
}

html_structure(airline_codes)
