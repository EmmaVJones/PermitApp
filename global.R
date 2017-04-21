library(shiny)
library(mapview)
library(sp)
library(shinyBS)
library(dplyr)
library(ggmap)


gageInfo <- readRDS('data/gageInfo.RDS')
coordinates(gageInfo) <- ~LongDD+LatDD
proj4string(gageInfo) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
