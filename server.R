source('global.R')

# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
Ecoregions <- readOGR('data/','vaECOREGIONlevel3__proj84')
Superbasins <- readOGR('data/','VAsuperbasins_proj84')



shinyServer(function(input, output, session) {
  activeDot <- function(map,x,y){addCircleMarkers(map,x,y,radius=6,color='blue',fillColor = 'yellow',
                                                  fillOpacity = 1,opacity=1,weight = 2,stroke=T,layerId = 'Selected')}
  
  ## Map ## Went with leaflet in the end over mapview
  output$GageMap <- renderLeaflet({
    if(input$targetlocation==""){
      leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
        addHomeButton(extent(gageInfo), "Virginia Gages")%>%
        addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
                         fillOpacity=0.5,group='gages',layerId=~GageNo,
                         popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
                                                             "HUC8","WebAddress")))}
    else{
      target_pos = geocode(input$targetlocation)

      leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
        setView(lng=target_pos$lon,lat=target_pos$lat,zoom=10)%>%
        #addHomeButton(extent(gageInfo), "Virginia Gages")%>%
        addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
                         fillOpacity=0.5,group='gages',layerId=~GageNo,
                         popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
                                                             "HUC8","WebAddress")))
        
    }
    
  })
  #output$GageMap <- renderMapview({
    #mapview(gageInfo, popup = popupTable(gageInfo, zcol = c("GageNo","StationName",
    #                                                        "DrainArea","HUC8","WebAddress")))})
  
  output$test <- renderPrint({input$targetlocation})

  
  ## Move map view to adjust with marker click ##
  observeEvent(input$GageMap_marker_click,{
    click <- input$GageMap_marker_click
    proxy <- leafletProxy("GageMap")
    if(click$id=="Selected"){
      proxy%>%removeMarker(layerId='Selected')
    }else{
      proxy %>% setView(lng=click$lng,
                        lat=ifelse(input$GageMap_zoom<10,click$lat+(3/input$GageMap_zoom),click$lat),
                        input$GageMap_zoom)%>%
        activeDot(click$lng,click$lat)
    }
  })
    
  
  ## Populate table on gage click ##
  observeEvent(input$GageMap_marker_click,{
    click<-input$GageMap_marker_click
    if(!is.null(click$id))
      output$gageText <- renderPrint({cat('Gage Number:',click$id)})
    # use cat() instead of paste() to not show line numbers
  })
  
  userGageSelection <- reactive({
    if(is.null(input$gageList))
      return(NULL)
    d <- subset(gageInfo@data,GageNo %in% input$gageList)
    names(d) <- c("Gage Number","Station Name","HUC8","Drainage Area","name2","WebAddress") 
    return(d[1:4,1:4])
  })
  
  output$gageInfoTable <- renderTable({
    if(is.null(userGageSelection()))
      NULL
    return(userGageSelection())
  })
  
  #output$test <- renderPrint({as.character(userGageSelection()[,1])})
  
  extraStats <- reactive({
    subset(gagestats,SITEID %in% as.character(userGageSelection()[,1])) %>%
      dplyr::select(SITEID:HARMEAN) # bc raster package is loaded
  })
  
  
  output$gageInfoTable2 <- renderTable({
    if(is.null(extraStats()))
      NULL
    return(extraStats())
    })
  
  
  
  
  
  
  ## Background Metals Weighted (Probmon Data) Section
  output$weightedMap <- renderLeaflet({
    leaflet(VAstationselect) %>% addProviderTiles('Thunderforest.Landscape') %>%
      fitBounds(~min(Longitude),~min(Latitude),~max(Longitude),~max(Latitude))%>%
      addMarkers(~Longitude,~Latitude,popup=~StationID,markerOptions(riseOnHover=T))
      
  })
 
  
  
  
  
  
  
  ## Background Metals UNweighted (all data) Section
  
  output$unweightedMap <- renderMapview({
    mapview( Ecoregions, zcol="US_L3NAME")+# popup = popupTable( eco2 , zcol = c("US_L3NAME")))+
      mapview(Superbasins,zcol="SUPERBASIN")#popup= popupTable( supaB2 , zcol = c("SUPERBASIN")))
    
    })
  
  
  
})

