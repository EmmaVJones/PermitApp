source('global.R')

# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
#Ecoregions <- readOGR('C:/HardDriveBackup/R/PermitTool/PermitApp/data','vaECOREGIONlevel3__proj84')
#Superbasins <- readOGR('C:/HardDriveBackup/R/PermitTool/PermitApp/data','VAsuperbasins_proj84')



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
  
  
  
  
  
  #-------------------------------------------------------------------------------------------
  ## Background Metals Weighted (Probmon Data) Section ##
  #-------------------------------------------------------------------------------------------
  output$weightedMap <- renderLeaflet({
    leaflet(metalsSites) %>% addProviderTiles('Thunderforest.Landscape') %>%
      fitBounds(~min(LongitudeDD),~min(LatitudeDD),~max(LongitudeDD),~max(LatitudeDD))%>%
      #fitBounds(metalsSites1@bbox)
      addMouseCoordinates()%>%addHomeButton(extent(metalsSites1), "Virginia")
  })
  
  # Subset metals CDF data based on user metal and subpopulation #
  metalsCDF_DataSelect <- reactive({
    if(input$metalToPlot=="No Metals")
      return(NULL)
    df <- filter(metalsCDF,Indicator==toupper(input$metalToPlot))
    if(input$subpopToPlot=="Virginia"){
      return(filter(df,Subpopulation=="Virginia"))}else{return(filter(df,Subpopulation==input$subpopToPlot))}
  })
  
  # Subset metals sites based on user metal and subpopulation #
  metalsSites_DataSelect <- reactive({
    if(is.null(metalsCDF_DataSelect()))
      return(NULL)
    df <- filter(metalsSites_long,metal==toupper(input$metalToPlot))
    if(input$subpopToPlot=="Virginia")
      return(filter(df,category=='Basin'))
    return(filter(df,Subpopulation==input$subpopToPlot))
   })
  
  
  popsummaryVA <- reactive({
    if(is.null(metalsCDF_DataSelect()))
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,"Virginia")
  })
  popsummarybasin <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$basin!="")
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,input$basin)
  })
  popsummarysuperBasin <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$superBasin!="")
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,input$superBasin)
  })
  popsummaryeco <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$ecoregion!="")
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,input$ecoregion)
  })
  popsummaryorder <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$order!="")
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,input$order)
  })
  
  #popsummaryALL <- reactive({
  #  if(is.null(metalsCDF_DataSelect()))
  #    return(NULL)
  #  if(input$basin)
    # use isolate to interactively add rows
  #})
  # Add markers to map based on user selection #
  observe({
    if(is.null(metalsCDF_DataSelect()))
      return(NULL)
    pal <- colorQuantile(c("#FDFEC5","#FEAF56","#FF4A31","#830025"), metalsCDF_DataSelect()$Value,n=4)
    
    leafletProxy('weightedMap',data=metalsSites_DataSelect()) %>% clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(data=metalsSites_DataSelect(),~LongitudeDD,~LatitudeDD,radius=6,
                       #color=~'blue',
                       color=~pal(metal_value),stroke=F,fillOpacity=0.5,
                       group='selectedSites',layerId=~StationID_Trend,
                       popup=paste(sep= "<br/>",metalsSites_DataSelect()$StationID,
                                   paste(capwords(tolower(metalsSites_DataSelect()$metal)),":",
                                   metalsSites_DataSelect()$metal_value,
                                   unique(metalsCDF_DataSelect()$units),sep=" ")))%>%
      addLegend("bottomright",pal=pal,values=~metalsCDF_DataSelect()$Value,
                title=paste(input$metalToPlot),opacity=1)
  })
  
  
  
  # Summary table of input dataset #
  output$weightedMetalsTable <- DT::renderDataTable({
    if(is.null(popsummaryVA()))
      return(NULL)
    datatable(popsummaryVA(),
              colnames=c('Metal','Subpopulation','n','5%','10%','25%','50%','75%','90%','95%'),
              extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',
                           buttons=list('copy')))
  })
  
  
  
  
  # Updating dissolved metals cdf plot
  #output$p_dMetal <- renderPlot({
  #  if(is.null(metalsCDF_DataSelect()))
  #    return(NULL)
  #  m <- max(metalsCDF_DataSelect()$NResp)
  #  
  #  p1 <- ggplot(metalsCDF_DataSelect(), aes(x=Value,y=Estimate.P)) + geom_point() + labs(x=as.character(pct1[1,1]),y="Percentile") +
  #    ggtitle(paste("Virginia",input$dMetal_,"\nPercentile Graph( n=",m,")",sep=" ")) + 
  #    theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
  #    theme(axis.title = element_text(face='bold',size=12))+
  #    geom_point(data=pct,color='orange',size=4)
    
    
    
  #  parametercap <- toupper(input$dMetal_)
  #  cdfsubset <- subFunction(cdfdata,parametercap,"Virginia")
  #  pct1 <- cbind(percentilesDissolvedMetals(),metal=sub(" .*","",percentilesDissolvedMetals()$Dissolved_Metal))%>%
  #    filter(metal==input$dMetal_)
  #  pct <- cbind(cdfsubset[1,1:3],Value=pct1$Measure,Estimate.P=as.numeric(as.character(pct1$Statewide_Percentile)),
  #               cdfsubset[1,6:8])
  #  #pct <- filter(cdfsubset,Value==pct1[,2])
  #  m <- max(cdfsubset$NResp)
  #  p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x=as.character(pct1[1,1]),y="Percentile") +
  #    ggtitle(paste("Virginia",input$dMetal_,"\nPercentile Graph( n=",m,")",sep=" ")) + 
  #    theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
  #    theme(axis.title = element_text(face='bold',size=12))+
  #    geom_point(data=pct,color='orange',size=4)
  #  
  #  std <- cbind(percentilesDissolvedMetals2()[,c(1,4)],metal=sub(" .*","",percentilesDissolvedMetals()$Dissolved_Metal))%>%
  #    filter(metal==input$dMetal_)
  #  
    
  #  if(input$addstd==F){return(p1)}else{
  #    if(is.na(std[1,2])){
  #      xloc <- 0.75*max(cdfsubset$Value)
  #      p1+annotate('text',x=xloc,y=50,label='No Criteria',color='red', fontface =2)}else{
  #        p1+geom_vline(xintercept=as.numeric(as.character(std[1,2])),color='red',linetype='dashed')}
  #  }},height = 250,width=325)
  
  
  
  
  
  
  ## Background Metals UNweighted (all data) Section
  
  #output$unweightedMap <- renderMapview({
  #  mapview( Ecoregions, zcol="US_L3NAME")+# popup = popupTable( eco2 , zcol = c("US_L3NAME")))+
  #    mapview(Superbasins,zcol="SUPERBASIN")#popup= popupTable( supaB2 , zcol = c("SUPERBASIN")))
  #  
  #  })
  
  
  
})

