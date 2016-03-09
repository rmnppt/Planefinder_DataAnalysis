# munging
library(readr)
library(dplyr)
files <- list.files("data/DEMODATA", full.names = T)

# # might be useful to have the times
# datetimes <- sub(".csv", "", list.files("data/DEMODATA"))
# extractSplit <- function(x, n) strsplit(x, " ")[[1]][n]
# dates <- lapply(datetimes, extractSplit, 1)
# times <- lapply(datetimes, extractSplit, 2)
# times <- sub("-", ":", times)
# datetimes <- as.POSIXct(paste(dates, times))

# need to assemble the files
flightsList <- list()
for(i in 1:length(files)) {
  flightsList[[i]] <- cbind(date = datetimes[i], read_csv(files[i]))
  cat("_", i, "_")
}

nRows <- sapply(flightsList, nrow)
nCols <- sapply(flightsList, ncol)
any(nCols != 14) 

cumRows <- c(0, cumsum(nRows))
flights <- as.data.frame(matrix(NA, sum(nRows), ncol(flightsList[[1]])))
for(i in 1:length(flightsList)) {
  theseRows <- (cumRows[i]+1):cumRows[i+1]
  flights[theseRows,] <- flightsList[[i]]
  cat("_", i, "_")
}

names(flights) <- names(flightsList[[1]])

saveRDS(flights, "data/assembled_flights.rds")
write.csv(flights, "data/assembled_flights.csv", row.names = F)
