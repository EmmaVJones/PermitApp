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
      output$gageText <- renderPrint({cat('Gage Currently Selected:',click$id)})
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
  
  # Subset metals CDF data based on user metal for stats below #
  metalsCDF_DataSelectfortable <- reactive({
    if(input$metalToPlot=="No Metals")
      return(NULL)
  filter(metalsCDF,Indicator==toupper(input$metalToPlot))})
  
  # Population report section
  popsummaryVA <- reactive({
    if(is.null(metalsCDF_DataSelect()))
      return(NULL)
    x <- filter(metalsCDF_DataSelectfortable(),Subpopulation=="Virginia")
    populationSummary(x,input$metalToPlot,"Virginia")
  })
  popsummarybasin <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$basin!="-")
      return(NULL)
    x <- filter(metalsCDF_DataSelectfortable(),Subpopulation==input$basin)
    populationSummary(x,input$metalToPlot,input$basin)
  })
  popsummarysuperBasin <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$superBasin!="-")
      return(NULL)
    x <- filter(metalsCDF_DataSelectfortable(),Subpopulation==input$superBasin)
    populationSummary(x,input$metalToPlot,input$superBasin)
  })
  popsummaryeco <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$ecoregion!="-")
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,input$ecoregion)
  })
  popsummaryorder <- reactive({
    if(is.null(metalsCDF_DataSelect())&input$order!="-")
      return(NULL)
    populationSummary(metalsCDF_DataSelect(),input$metalToPlot,input$order)
  })
  popsummaryALL <- reactive({
    if(is.null(metalsCDF_DataSelect()))
      return(NULL)
    x <- popsummaryVA()
    if(input$basin!="-")
      x <- rbind(x,popsummarybasin())
    if(input$superBasin!="-")
      x <- rbind(x,popsummarysuperBasin())
    if(input$ecoregion!="-")
      x <- rbind(x,popsummaryeco())
    if(input$order!="-")
      x <- rbind(x,popsummaryorder())
    return(x)
  })
  
  
  
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
    if(is.null(popsummaryALL()))
      return(NULL)
    datatable(popsummaryALL(),
              colnames=c('Metal','Subpopulation','n','5%','10%','25%','50%','75%','90%','95%'),
              extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',
                           buttons=list('copy',
                           list(extend='csv',filename=paste('BackgroundMetals_',input$metalToPlot,Sys.Date(),sep='')),
                           list(extend='excel',filename=paste('BackgroundMetals_',input$metalToPlot,Sys.Date(),sep='')))))})                         
  
  # All Stats Summary table #
  allstats <- reactive({
    subset(allstatsdata,Subpopulation %in% c("Virginia",input$basin,input$superBasin,
                                             input$ecoregion,input$order))
    
  })
  
  observeEvent(input$reviewstats,{
    showModal(modalDialog(
      title="Statistics for all metals (Preview)",
      h5("For faster app rendering, this is just a preview  of your selected data.",
         span(strong("Click the 'Send to Report' button to review the entire dataset."))),
      hr(),
      tableOutput('allstatstable'),
      br(),
     downloadButton('knit','Send to Report'),
      easyClose = TRUE
    ))
  })
  
  output$allstatstable <- renderTable({
    df <- allstats()[1:10,1:7]
    df$n <- format(df$n,digits=1)
    names(df) <- c('Metal','Subpopulation','n','5%','10%','25%','50%')
    return(df)})
  
  

  # Subset data on user map click to highlight on CDF curve
  marker <- reactive({
    click <- input$weightedMap_marker_click
    if(is.null(click))
      return()
    return(click$id)
  })
  
  # Updating dissolved metals cdf plot
  wCDFplot <- reactive({
    if(is.null(metalsCDF_DataSelect()))
      return(NULL)
    m <- max(metalsCDF_DataSelect()$NResp)
    xaxis <- as.character(paste(capwords(tolower(metalsCDF_DataSelect()$Indicator[1])),' (',metalsCDF_DataSelect()$units[1],')'))
    p <- ggplot(metalsCDF_DataSelect(), aes(x=Value,y=Estimate.P)) + geom_point() + labs(x=xaxis,y="Percentile") +
      ggtitle(as.character(paste(capwords(tolower(metalsCDF_DataSelect()$Indicator[1])),'in \n',
                                 metalsCDF_DataSelect()$Subpopulation[1],'\n (n=',m,')'))) + 
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
      theme(axis.title = element_text(face='bold',size=12))+ 
      geom_ribbon(data=metalsCDF_DataSelect(),aes(ymin=LCB95Pct.P,ymax=UCB95Pct.P),alpha=0.3)
    if(is.null(marker())){
      return(p)
    }else{
      xspot <- max(metalsCDF_DataSelect()$Value)*0.7
      dat <- subset(metalsSites,StationID_Trend %in% as.character(marker()))%>%
        select_(toupper(input$metalToPlot))
      cdf <- select(metalsCDF_DataSelect(),Value,Estimate.P)
      df <- data.frame(Value=dat[[1]],Estimate.P=vlookup(dat[[1]],cdf,2,TRUE))
      return(p+geom_point(data=df,aes(x=Value,y=Estimate.P),color='orange',size=4)+
               annotate("text",label=paste(sep= "\n","Percentile Highlighted \nfor StationID:",marker()),
                        x=xspot,y=10,hjust=0,vjust=0))}
  })
  output$weightedMetalsCDF <- renderPlot({wCDFplot()})
  
  ##---------------------------------------RMARKDOWN SECTION----------------------------------------------
  
  output$knit <- downloadHandler(
    'results.html',
    content= function(file){
      tempReport <- file.path(tempdir(), "reportHTML.Rmd")
      file.copy("reportHTML.Rmd",tempReport,overwrite=T)
      params <- list(table_allstats=allstats())
      
      rmarkdown::render(tempReport,output_file= file,
                        params=params, envir=new.env(parent=globalenv()))})
  ##------------------------------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------------------
  ## Background Metals UNweighted (all data) Section ##
  #-------------------------------------------------------------------------------------------
  output$unweightedMap <- renderLeaflet({
    if(input$targetlocationUN==""){
      leaflet() %>% addProviderTiles('Thunderforest.Landscape') %>%
        addMouseCoordinates()%>%addHomeButton(extent(metalsSites1), "Virginia")%>%
        addCircleMarkers(data=metalsSites1,radius=6,
                         color=~'black',stroke=F,fillOpacity=0.5,
                         group='selectedSites_UN',layerId=~StationID_Trend,
                         popup=popupTable(metalsSites1, zcol = c("StationID","Year","StationID_Trend","CALCIUM","MAGNESIUM","ARSENIC","BARIUM",         
                                                                "BERYLLIUM","CADMIUM","CHROMIUM","COPPER","IRON","LEAD","MANGANESE","THALLIUM",
                                                                "NICKEL","SILVER","ZINC","ANTIMONY","ALUMINUM","SELENIUM","HARDNESS","MERCURY")))
    }else{
      target_pos = geocode(input$targetlocationUN)
      
      leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
        setView(lng=target_pos$lon,lat=target_pos$lat,zoom=10)%>%
        addCircleMarkers(data=metalsSites1,radius=6,
                         color=~'black',stroke=F,fillOpacity=0.5,
                         group='selectedSites_UN',layerId=~StationID_Trend,
                         popup=popupTable(metalsSites1, zcol = c("StationID","Year","StationID_Trend","CALCIUM","MAGNESIUM","ARSENIC","BARIUM",         
                                                                "BERYLLIUM","CADMIUM","CHROMIUM","COPPER","IRON","LEAD","MANGANESE","THALLIUM",
                                                                "NICKEL","SILVER","ZINC","ANTIMONY","ALUMINUM","SELENIUM","HARDNESS","MERCURY")))
      }
                           
                           
    
      
  })
  
  #output$GageMap <- renderLeaflet({
  #  if(input$targetlocationUN==""){
  #    leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
  #      addHomeButton(extent(gageInfo), "Virginia Gages")%>%
  #      addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
  #                       fillOpacity=0.5,group='gages',layerId=~GageNo,
  #                       popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
  #                                                           "HUC8","WebAddress")))}
  #  else{
  #    target_pos = geocode(input$targetlocation)
  #    
  #    leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%
  #      setView(lng=target_pos$lon,lat=target_pos$lat,zoom=10)%>%
  #      #addHomeButton(extent(gageInfo), "Virginia Gages")%>%
  #      addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
  #                       fillOpacity=0.5,group='gages',layerId=~GageNo,
  #                       popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
  #                                                           "HUC8","WebAddress")))
      
  #  }
    
  #})
  
  
  
  
  
  # Subset metals unweighted data based on user metal #
  metalsUN_DataSelect <- reactive({
    if(input$metalToPlotUN=="No Metals")
      return(NULL)
    df <- filter(metalsSites_long,metal==toupper(input$metalToPlotUN))
  })
  
  
  # Add markers to map based on user selection #
  #observe({
  #  if(is.null(metalsUN_DataSelect()))
  #    return(NULL)
  #  
  #  leafletProxy('unweightedMap',data=metalsUN_DataSelect()) %>% clearMarkers() %>%
  #    clearControls() %>%
  #    addCircleMarkers(data=metalsUN_DataSelect(),~LongitudeDD,~LatitudeDD,radius=6,
  #                     color=~'black',stroke=F,fillOpacity=0.5,
  #                     #color=~pal(metal_value),
  #                     group='selectedSites_UN',layerId=~StationID_Trend,
  #                     popup=paste(sep= "<br/>",metalsUN_DataSelect()$StationID,
  #                                 paste(capwords(tolower(metalsUN_DataSelect()$metal)),":",
  #                                       metalsUN_DataSelect()$metal_value,
  #                                       unique(metalsUN_DataSelect()$units),sep=" ")))
  #})
  
  # Subset metals sites based on user metal and subpopulation #
  #metalsSites_DataSelect <- reactive({
  #  if(is.null(metalsCDF_DataSelect()))
  #    return(NULL)
  #  df <- filter(metalsSites_long,metal==toupper(input$metalToPlot))
  #  if(input$subpopToPlot=="Virginia")
  #    return(filter(df,category=='Basin'))
  #  return(filter(df,Subpopulation==input$subpopToPlot))
  #})
  
  
  
  #output$unweightedMap <- renderMapview({
  #  mapview( Ecoregions, zcol="US_L3NAME")+# popup = popupTable( eco2 , zcol = c("US_L3NAME")))+
  #    mapview(Superbasins,zcol="SUPERBASIN")#popup= popupTable( supaB2 , zcol = c("SUPERBASIN")))
  #  
  #  })
  
  
  
})

