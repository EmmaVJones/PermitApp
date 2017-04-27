metalsSites_DataSelect <- filter(metalsSites_long,metal=='CALCIUM')%>%
  filter(category=='Basin')


metalsCDF_DataSelect <- filter(metalsCDF,Indicator=="CALCIUM")%>%
  filter(Subpopulation=='Virginia')


stats <- populationSummary(metalsCDF_DataSelect)

tdf <- as.data.frame(t(stats[,2:8]),row.names=1:7)

pal <- colorQuantile("Reds", c(min(metalsCDF_DataSelect$Value),max(metalsCDF_DataSelect$Value)), n = 4)
pal <- colorQuantile("Reds", c(min(metalsCDF_DataSelect$Value),max(metalsCDF_DataSelect$Value)),
                     probs=)

pal <- colorBin("Reds", c(min(metalsCDF_DataSelect$Value),max(metalsCDF_DataSelect$Value)), n = 2)

previewColors(colorBin("Reds", c(min(metalsCDF_DataSelect$Value),max(metalsCDF_DataSelect$Value)),
                       n=2),sort(metalsSites_DataSelect$metal_value))


previewColors(colorQuantile("Reds", metalsCDF_DataSelect$Value,
                            n=4),sort(metalsSites_DataSelect$metal_value))
pal <- colorQuantile("Reds", metalsCDF_DataSelect$Value,
                     n=4)
#
#pal <- colorQuantile("Reds", tdf, n = 4)
#pal <- colorNumeric(palette = "Reds",domain = c(metalsCDF_DataSelect()$x5,
#                                                 metalsCDF_DataSelect()$x10,
#                                                 metalsCDF_DataSelect()$x25,
#                                                 metalsCDF_DataSelect()$x50,
#                                                 metalsCDF_DataSelect()$x75,
#                                                 metalsCDF_DataSelect()$x90,
#                                                 metalsCDF_DataSelect()$x95))
#c(5,10,25,50,75,90,95))


previewColors(colorNumeric("Blues", domain = NULL), sort(rexp(16)))
# Exponential distribution, mapped by interval
previewColors(colorBin("Blues", domain = NULL, bins = 4), sort(rexp(16)))
# Exponential distribution, mapped by quantile
previewColors(colorQuantile("Blues", domain = NULL), sort(rexp(16)))







#plotting Zone


metalsSites_DataSelect <- filter(metalsSites_long,metal=='CALCIUM')%>%
  filter(category=='Basin')


metalsCDF_DataSelect <- filter(metalsCDF,Indicator=="CALCIUM")%>%
  filter(Subpopulation=='Virginia')


previewColors(colorBin(c("#FDFEC5","#FEAF56","#FF4A31","#830025"),
                            metalsCDF_DataSelect$Value,n=4),
              sort(metalsSites_DataSelect$metal_value))

pal <- colorQuantile(c("#FDFEC5","#FEAF56","#FF4A31","#830025"),
                     metalsCDF_DataSelect$Value,n=4)
leaflet(data=metalsSites_DataSelect) %>%addProviderTiles('Thunderforest.Landscape')%>% 
  clearControls() %>% clearMarkers() %>%
  addCircleMarkers(data=metalsSites_DataSelect,~LongitudeDD,~LatitudeDD,radius=6,
                   #color=~'blue',
                   color=~pal(metal_value),stroke=F,
                   fillOpacity=0.5,group='selectedSites',layerId=~StationID_Trend,
                   popup=~StationID)%>%
  addLegend('bottomright',title='metal concentration',pal=pal,
            values=~metalsCDF_DataSelect$Value)
  

addLegend(pal=pal,values=values,labFormat=function(type,cuts,p){n=length(cuts)
  paste0(cuts[-n]," &ndash;",cuts[-1])
  })



paste(sep= "<br/>",metalsSites_DataSelect()$StationID,
      paste(capwords(metalsSites_DataSelect()$metal),":",
            metalsSites_DataSelect()$metal_value,
            unique(metalsCDF_DataSelect()$units),sep=" "))