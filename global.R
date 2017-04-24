library(shiny)
library(mapview)
library(raster)
library(shinyBS)
library(dplyr)
library(shinyFiles)
library(ggmap)
library(rgdal)
library(htmltools)



gageInfo <- readRDS('data/gageInfo.RDS')
coordinates(gageInfo) <- ~LongDD+LatDD
proj4string(gageInfo) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 


gagestats <- read.csv('data/allflowstatsComparison.csv')
gagestats$SITEID <- paste0("0",gagestats$SITEID)


# Prob metals data
metalsCDF <- readRDS('data/metalsCDF_March2017Update.RDS')


# Bring in Virginia point data
VAstationselect <- readRDS('data/VAstationselectMarch2017update_final.RDS')
