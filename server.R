source('global.R')


shinyServer(function(input, output, session) {
  activeDot <- function(map,x,y){addCircleMarkers(map,x,y,radius=6,color='blue',fillColor = 'yellow',
                                                  fillOpacity = 1,opacity=1,weight = 2,stroke=T,layerId = 'Selected')}
  
  ## Map ## Went with leaflet in the end over mapview
  output$GageMap <- renderLeaflet({
    leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
      addHomeButton(extent(gageInfo), "Virginia Gages")%>%
      addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
                      fillOpacity=0.5,group='gages',layerId=~GageNo,
                 popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
                                                     "HUC8","WebAddress")))
  })
  #output$GageMap <- renderMapview({
    #mapview(gageInfo, popup = popupTable(gageInfo, zcol = c("GageNo","StationName",
    #                                                        "DrainArea","HUC8","WebAddress")))})
  
  
  ## Zoom Button ## broken for now 
  eventReactive(input$zoomButton,{
    #zoompoint <- 'userpoint'
    #coordinates(zoompoint) <- ~input$long+input$lat
    #proj4string(zoompoint) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
    #mapview(zoompoint)
    leafletProxy({'GageMap'})%>%setView(input$long,input$lat,zoom=5)
  })
  
  
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
      output$gageText <- renderPrint({paste('Gage Number:',click$id)})
  })
  
  output$gageInfoTable <- renderTable({
    if(is.null(input$gageList))
      return(NULL)
    d <- subset(gageInfo@data,GageNo %in% input$gageList)
    names(d) <- c("Gage Number","Station Name","HUC8","Drainage Area","name2","WebAddress") 
    return(d[1:4,1:4])
  })
  
  
  
  
})

