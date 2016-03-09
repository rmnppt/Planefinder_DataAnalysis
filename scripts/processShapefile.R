# process shapefile data
library(rgdal) # requires sp, will use proj.4 if installed
library(gpclib)
library(maptools)
library(ggplot2)
library(plyr)

gpclibPermit()
shpToDf <- function(directory, layer) {
  map <- readOGR(dsn=directory, layer=layer)
  map@data$id <- rownames(map@data)
  map.points <- fortify(map, region = "id")
  map.df <- join(map.points, map@data, by = "id")
}

# thin <- function(dat, factor) {
#   remove <- rep(1, length.out = nrow(dat))
#   for(i in 1:length(remove)) {
#     if(i %% factor == 0) {
#       remove[i] <- 0
#     }
#   }
#   dat[-remove == 0,]
# }
# 
# map.df.thin <- thin(map.df, 100)

US.df <- shpToDf("data/cb_2014_us_state_5m", "cb_2014_us_state_5m")
saveRDS(US.df, "data/USAShape.rds")

world.df <- shpToDf("data/gshhg-shp-2.3.4/GSHHS_shp/c", "GSHHS_c_L1")
saveRDS(world.df, "data/worldShape.rds")

asia.df <- shpToDf("data/asia-natural-shape/", "natural")
saveRDS(asia.df, "data/asiaShape.rds")
