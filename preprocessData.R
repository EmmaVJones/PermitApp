library(plyr)
library(dplyr)
library(reshape2)



# Prob metals data, from Benthic TMDL App, finally fixing issues
metalsCDF <- readRDS('data/metalsCDF_March2017Update.RDS')
metalsCDF$Indicator <- gsub("CALCUIM","CALCIUM",metalsCDF$Indicator)
metalsCDF$Indicator <- gsub("BERY","BERYLLIUM",metalsCDF$Indicator)
metalsCDF$Indicator <- gsub("MAGN", "MAGNESIUM",metalsCDF$Indicator)
metalsCDF$Subpopulation <- gsub("Rappahanock", "Rappahannock",metalsCDF$Subpopulation)
metalsCDF$Subpopulation <- gsub("James Basin", "James",metalsCDF$Subpopulation)
metalsCDF$Subpopulation <- gsub("Roanoke Basin", "Roanoke",metalsCDF$Subpopulation)
metalsCDF <- filter(metalsCDF,!(Subpopulation %in% c("Bay Watersheds 2001-2007","Bay Watersheds 2008-2014",
                                                    "IR2008","IR2010","IR2012","IR2014","IR2016",
                                                    "Non-Bay Watersheds 2001-2007","Non-Bay Watersheds 2008-2014",
                                                    "VSCI Scores 2001-2003","VSCI Scores 2004-2006",                 
                                                    "VSCI Scores 2007-2010","VSCI Scores 2011-2014",                 
                                                    "Year 2003","Year 2004","Year 2005","Year 2006",                            
                                                    "Year 2007","Year 2008","Year 2009","Year 2010",                             
                                                    "Year 2011","Year 2012","Year 2013","Year 2014")))
metalsCDF[,1] <- as.factor(metalsCDF[,1])
metalsCDF[,2] <- as.factor(metalsCDF[,2])
metalsCDF[,3:12] <- apply(metalsCDF[,3:12],2,function(x) as.numeric(as.character(x)))


metalsCDF <- mutate(metalsCDF,units=Indicator)
metalsCDF$units <- dplyr::recode(metalsCDF$units,"VSCIAll"="(unitless)","DChloride"="mg/L","DO"="mg/L","DPotassium"="mg/L","DSodium"="mg/L","DSulfate"="mg/L",
                                "LRBS"="(unitless)", "MetalsCCU"="(unitless)","pH"="(unitless)","SpCond"="uS/cm","TDS"="mg/L",
                                "TN"="mg/L","TotalHabitat"="(unitless)","TP"="mg/L","ANTIMONY"="ug/L","ALUMINUM"="ug/L",
                                "ARSENIC"="ug/L","BARIUM"="ug/L","BERYLLIUM"="ug/L","CADMIUM"="ug/L","CALCIUM"="mg/L","CHROMIUM"="ug/L",
                                "COPPER"="ug/L","IRON"="ug/L","LEAD"="ug/L","MAGNESIUM"="mg/L","MANGANESE"="ug/L","NICKEL"="ug/L",
                                "SELENIUM"="ug/L","SILVER"="ug/L","THALLIUM"="ug/L","ZINC"="ug/L","HARDNESS"="mg/L")    

# get rid of some subpopulation levels

saveRDS(metalsCDF,'data/metalsCDF.RDS')


# ProbMetrics_2001-2014_Final_Web_March_3_2017.xlsx taken from Benthic TMDL App
metalsSites <- readxl::read_excel('data/ProbMetrics_2001-2014_Final_Web_March_3_2017.xlsx',
                                  sheet='Final_ProbMetrics')%>%
  filter(Year>2002) %>% # get only metals years
  select(StationID,Year,StationID_Trend,LongitudeDD,LatitudeDD,Basin,SubBasin,EcoRegion,Order,
         AREA_SQ_MILES,CALCIUM:HARDNESS)
metalsSites$Order[metalsSites$Order=="1"] <- "First Order"
metalsSites$Order[metalsSites$Order=="2"] <- "Second Order"
metalsSites$Order[metalsSites$Order=="3"] <- "Third Order"
metalsSites$Order[metalsSites$Order=="4"] <- "Fourth Order"
metalsSites$Order[metalsSites$Order=="5"|metalsSites$Order=="6"] <- "Fifth Order"

metalsSites$Order <- as.factor(metalsSites$Order)

test <- melt(metalsSites,id.vars=c("StationID","Year","StationID_Trend","LongitudeDD","LatitudeDD","AREA_SQ_MILES"),
             measure.vars=c("CALCIUM","MAGNESIUM","ARSENIC","BARIUM","BERYLLIUM","CADMIUM","CHROMIUM","COPPER",
                            "IRON", "LEAD","MANGANESE","THALLIUM","NICKEL","SILVER","ZINC","ANTIMONY","ALUMINUM",
                            "SELENIUM","HARDNESS"))
test2 <- melt(metalsSites,id.vars=c("StationID","Year","StationID_Trend","LongitudeDD","LatitudeDD","AREA_SQ_MILES"),
             measure.vars=c("Basin","SubBasin","EcoRegion","Order"))

#metalsSites_long <- merge(test,metalsSites[,1:10],by=c("StationID","Year","StationID_Trend","LongitudeDD","LatitudeDD","AREA_SQ_MILES"))
     

metalsSites_long <- plyr::join(test,test2,by=c("StationID","Year","StationID_Trend","LongitudeDD","LatitudeDD","AREA_SQ_MILES"))
names(metalsSites_long) <- c("StationID","Year","StationID_Trend","LongitudeDD","LatitudeDD","AREA_SQ_MILES",
                             "metal","metal_value","category","Subpopulation")
metalsSites_long$StationID <- as.factor(metalsSites_long$StationID)
metalsSites_long$StationID_Trend <- as.factor(metalsSites_long$StationID_Trend)
metalsSites_long$Subpopulation <- as.factor(metalsSites_long$Subpopulation)

saveRDS(metalsSites,'data/MetalsSites.RDS')
saveRDS(metalsSites_long,'data/MetalsSites_long.RDS')


# Update all stats to easily call in the app
df <- data.frame(Metal=NA,Subpopulation=NA,n=NA,x5=NA,x10=NA,x25=NA,x50=NA,x75=NA,x90=NA,x95=NA)
datalist <- list()

for(i in 1:length(levels(metalsCDF$Indicator))){
  onemetal <- filter(metalsCDF,Indicator==as.character(levels(metalsCDF$Indicator)[i]))
  for(k in 1:length(levels(metalsCDF$Subpopulation))){
    ind <- as.character(onemetal$Indicator[1])
    sub <- as.character(levels(metalsCDF$Subpopulation)[k])
    onemetalonepop <- filter(onemetal,Subpopulation==sub)
    df1 <- populationSummary(onemetalonepop,ind,sub)%>%
      mutate(Metal=ind,Subpopulation=sub)%>%select(Metal,Subpopulation,everything())
    df[k,] <- df1
  }
  datalist[[i]] <- df
}
allstatsdata <- do.call(rbind,datalist)
saveRDS(allstatsdata,'data/allstatsdata.RDS')
