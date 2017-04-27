library(shiny)
library(mapview)
library(raster)
library(shinyBS)
library(dplyr)
library(shinyFiles)
library(ggmap)
library(rgdal)
library(htmltools)
library(DT)


## Data

gageInfo <- readRDS('data/gageInfo.RDS')
coordinates(gageInfo) <- ~LongDD+LatDD
proj4string(gageInfo) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 


gagestats <- read.csv('data/allflowstatsComparison.csv')
gagestats$SITEID <- paste0("0",gagestats$SITEID)


# Prob metals data, from Benthic TMDL App, finally fixed issues in preprocessData.R
metalsCDF <- readRDS('data/metalsCDF.RDS')




# Bring in Virginia point data
metalsSites <- readRDS('data/MetalsSites.RDS')
#make it a spatial dataset for home button to work
metalsSites1 <- metalsSites 
coordinates(metalsSites1) <- ~LongitudeDD+LatitudeDD
proj4string(metalsSites1) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 

metalsSites_long <- readRDS('data/MetalsSites_long.RDS')



## Functions

# Capitalize first letter in a word easily
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}



# VLOOKUP (Excel function hack) by Julin Maloof
vlookup <- function(ref, #the value or values that you want to look for
                    table, #the table where you want to look for it; will look in first column
                    column, #the column that you want the return data to come from,
                    range=FALSE, #if there is not an exact match, return the closest?
                    larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}


## Summarize input population, from cdf dataset so can't use summary()
populationSummary <- function(dataset,metal,subpopulation){
  # need dataset in particular format to search the right column with VLOOKUP hack
  dataset2 <- select(dataset,Estimate.P,Value)
  
  stats <- data.frame(Metal=metal,Subpopulation=subpopulation,
                      n=max(dataset$NResp),x5=vlookup(5,dataset2,2,range=T),
                      x10=vlookup(10,dataset2,2,range=T),x25=vlookup(25,dataset2,2,range=T),
                      x50=vlookup(50,dataset2,2,range=T),x75=vlookup(75,dataset2,2,range=T),
                      x90=vlookup(90,dataset2,2,range=T),x95=vlookup(95,dataset2,2,range=T))
  return(stats)
}
