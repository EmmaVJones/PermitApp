library(shiny)
library(mapview)
library(leaflet)
library(raster)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(shinyFiles)
library(ggmap)
library(rgdal)
library(htmltools)
library(DT)
library(tidyr)
library(ggplot2)
library(dataRetrieval)


## Data
template <- read.csv('data/template.csv')

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


allstatsdata <- readRDS('data/allstatsdata.RDS')
allstatsdataUN <- readRDS('data/allstatsdataUN.RDS')


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
#latlong <-'36.65, -78.965'
#watershedlevel <- huc8
#metalOfInterest <- 'magnesium'

## Geographic subset and unweighted statistics ##
geogsub <- function(latlong,watershedlevel,metalOfInterest){
  # assign lat/long values and remove any spaces from the string
  lat <- as.numeric(gsub(" ","",strsplit(latlong,",")[[1]][1]))
  lng <- as.numeric(gsub(" ","",strsplit(latlong,",")[[1]][2]))
  # make a spatial object from lat/long
  point <- data.frame(name='userPoint',lat=lat,lng=lng)
  coordinates(point) <- ~lng+lat
  proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")        
  # identify which polygon to clip metalsSites1 based on point location
  poly<- watershedlevel[point,]
  if(nrow(poly@data)==0){return(NULL)}else{
    #  clip metalsSites1 to poly
    metalsSites2 <- metalsSites1[poly,]
    # subset metalsSites_long by sites identified by watershed clip
    toclip <- metalsSites2@data$StationID
    metalsSites3 <- filter(metalsSites_long,metal==toupper(metalOfInterest),
                           StationID %in% toclip,category=='Basin') #just get one of each
    x <- as.data.frame(signif(quantile(metalsSites3$metal_value,
                                       probs=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),
                                       na.rm=T),3))
    x <- mutate(x,Percentile=rownames(x))
    names(x)[1] <- 'Value'
    # rearrange so it makes sense
    xwide <- spread(x,Percentile,Value)%>%select(one_of("5%","10%","25%", "50%","75%","90%","95%"))%>%
      mutate(Watershed=poly@data$NAME,n=sum(!is.na(metalsSites3$metal_value)))%>%select(Watershed,n,everything())
    if(length(poly@data)>2){
      xwide$Watershed <- paste(xwide$Watershed," (",poly@data$CU,")",sep="")}
      #xwide <- mutate(xwide,HUC8=poly@data$CU)%>%select(Watershed,HUC8,everything())}
    return(xwide)}
  
  }

#geogsub('36.8, -78.965',huc8,'magnesium')
#geogsub('37.265, -78.56',Ecoregions,'iron')
#geogsub('37.265, -78.56',Superbasins,'iron')


## ggplot regression model and plot
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red",formula= (y ~ exp(x))) +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression1 <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
