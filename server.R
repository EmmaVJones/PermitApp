source('global.R')


shinyServer(function(input, output, session) {
  
  output$GageMap <- renderLeaflet({
    leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
      addHomeButton(extent(gageInfo), "Virginia Gages")%>%
      addMarkers(data=gageInfo,layerId=nrow(gageInfo),
                 popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
                                                     "HUC8","WebAddress")))
  })
  #output$GageMap <- renderMapview({
    #mapview(gageInfo, popup = popupTable(gageInfo, zcol = c("GageNo","StationName",
    #                                                        "DrainArea","HUC8","WebAddress")               
    #                                     ))})
  
  # dir
  shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('', 'txt'))
  dir <- reactive(input$dir)
  output$dir <- renderPrint(dir())
  
  # path
  path <- reactive({
    home <- normalizePath("~")
    file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  })
  
  # files
  output$files <- renderPrint(list.files(path()))
  
  eventReactive(input$submitZoom,{
    #zoompoint <- 'userpoint'
    #coordinates(zoompoint) <- ~input$long+input$lat
    #proj4string(zoompoint) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")                                 
    #mapview(zoompoint)
    leafletProxy({'GageMap'})%>%setView(input$long,input$lat,zoom=5)
    
  })
  
})

