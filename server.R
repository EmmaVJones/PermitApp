source('global.R')
# Change back to style=basic






# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
#Ecoregions <- readOGR('data','vaECOREGIONlevel3__proj84')
#Ecoregions@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#Superbasins <- readOGR('data','VAsuperbasins_proj84')
#Superbasins@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#huc8 <- readOGR('data','HUC8_wgs84')
#huc8@proj4string <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


shinyServer(function(input, output, session) {
  activeDot <- function(map,x,y){addCircleMarkers(map,x,y,radius=6,color='blue',fillColor = 'yellow',
                                                  fillOpacity = 1,opacity=1,weight = 2,stroke=T,layerId = 'Selected')}
  
  ## Map ## Went with leaflet in the end over mapview
  output$GageMap <- renderLeaflet({
    if(input$targetlocation==""){
      leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%#style='basic')%>%
        addHomeButton(extent(gageInfo), "Virginia Gages")%>%
        addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
                         fillOpacity=0.5,group='gages',layerId=~GageNo,
                         popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
                                                             "HUC8","WebAddress")))}
    else{
      target_pos = geocode(input$targetlocation)

      leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%#style='basic')%>%
        setView(lng=target_pos$lon,lat=target_pos$lat,zoom=10)%>%
        #addHomeButton(extent(gageInfo), "Virginia Gages")%>%
        addCircleMarkers(data=gageInfo,radius=6,color=~'blue',stroke=F,
                         fillOpacity=0.5,group='gages',layerId=~GageNo,
                         popup=popupTable(gageInfo, zcol = c("GageNo","StationName","DrainArea",
                                                             "HUC8","WebAddress")))
        
    }
    
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
  ## Take click info from map ##
  observeEvent(input$GageMap_marker_click,{
    click<-input$GageMap_marker_click
    if(!is.null(click$id))
      output$gageText <- renderPrint({cat('Gage Currently Selected:',click$id)})
    # use cat() instead of paste() to not show line numbers
  })
  
  ## Use click info from map to subset gageInfo ##
  userGageSelection <- reactive({
    if(is.null(input$gageList))
      return(NULL)
    d <- subset(gageInfo@data,GageNo %in% input$gageList)
    names(d) <- c("Gage Number","Station Name","HUC8","Drainage Area","name2","WebAddress") 
    return(d[1:4,1:4])
  })
  
  ## Display subset of gageInfo ##
  output$gageInfoTable <- renderTable({
    if(is.null(userGageSelection()))
      NULL
    return(userGageSelection())
  })
  
  ## Select all stats from gagestats table based on gageInfo site subset on next tab ##
  extraStats <- reactive({
    flowstats <- subset(gagestats,SITEID %in% as.character(userGageSelection()[,1])) %>%
      dplyr::select(SITEID:HARMEAN) # bc raster package is loaded
    gageStats <- subset(gageInfo@data,GageNo %in% as.character(userGageSelection()[,1]))%>%
      mutate(SITEID=GageNo)%>%select(-c(name2,GageNo))
    merge(gageStats,flowstats,by='SITEID')
  })
  
  ## Display all stats from gagestats table based on gageInfo site subset on next tab ##
  output$gageInfoTable2 <- renderTable({
    if(is.null(extraStats()))
      NULL
    return(extraStats())
    })
  
  #-------------------------------------------------------------------------------------------
  ## Existing Gage Correction Section
  #-------------------------------------------------------------------------------------------
  
  ## UI component with MathJax ##
  output$formulas <- renderUI({
    fluidRow(
      column(8,
             fluidRow(#tags$head(tags$style(type="text/css","label.control-label, .text-control.single{ display: table-cell; text-align: center; vertical-align: middle; }  .form-group { display: table-row;}")),
               column(8,textInput('userFormula','y = ', placeholder = 'Example: 0.008')),column(2,p('x'))),
               textInput('power',"Type power below",placeholder='Example: 1.0854')))#,
      #column(4,plotOutput('formulaPlot')))
             #withMathJax(textOutput('formula'))))
  })
  
  ## Grey out updateFlowStats button until userFormula and power are filled in if under 'Add Formula' radiobutton
  observe({
    if(input$addFormula == "Add Formula" && input$userFormula !="" && input$power != "" ||
       input$addFormula=="No Correction"){
      shinyjs::enable("updateFlowStats")}else{shinyjs::disable("updateFlowStats")}
    #shinyjs::toggleState('updateFlowStats', input$addFormula == "Add Formula" && input$userFormula !="" && input$power != "")
  })
  
  ## Adjust flow statistics based on user options ##
  updatedFlowStats <- eventReactive(input$updateFlowStats,{
    step1 <- subset(gagestats,SITEID %in% as.character(input$gageListupdatestats)) %>%
      dplyr::select(SITEID:HARMEAN)
    if(input$addFormula=='No Correction'){
      step1.1 <- as.data.frame(t(step1))
      names(step1.1) <- "Selected Gage"
      return(step1.1)
      }else{
        step1[2,] <- c(SITEID=NA,sapply(step1[,2:7],function(x) (as.numeric(input$userFormula)*(x^as.numeric(input$power)))))
        rownames(step1) <- c("Selected Gage","User Correction")
        step2 <- as.data.frame(t(step1))
        return(step2)}
    
  })
  
  output$adjustedFlowStats <- renderDataTable({
    if(is.null(updatedFlowStats()))
      return(NULL)
    datatable(updatedFlowStats(),rownames = T,extensions = 'Buttons', escape=F,
              options=list(dom='Bt',
                           buttons=list('copy',
                                        list(extend='csv',filename=paste('UpdatedFlowStatistics_',input$gageListupdatestats,Sys.Date(),sep='')),
                                        list(extend='excel',filename=paste('UpdatedFlowStatistics_',input$gageListupdatestats,Sys.Date(),sep='')))))
    
  })
  
  
  
  ## Display user input formula ##
  #output$formulaPlot <- renderPlot({
  #  if(is.null(input$userFormula))
  #    return(NULL)
  #  ex <- paste(input$userFomula,"^",input$exponent)
  #  ggplot(data.frame(x=c(0,10)),aes(x))+
  #    stat_function(fun=function(x)0.0009*x^1.8013,geom='line')+
  #    scale_x_log10()+scale_y_log10()+
  #    annotation_logticks()+
  #    annotate('text',x=10,y=1.5,label=ex, parse=T)
      #annotate('text',x=2,y=3,label=expression(Value~is~sigma~R^{2}==0.6))
  #})
  
  #output$formula <- renderPrint({
  #  if(is.null(input$userFormula))
  #    return(NULL)
    #withMathJax(('$$input$userFormula^2$$'))
    #withMathJax($$\\alpha^2$$)
    #expression(input$userFormula ^ input$exponent)
    #tags$div(HTML(paste(input$userFormula,tags$sup(input$exponent),sep="")))
  #})
  
  
  
  #-------------------------------------------------------------------------------------------
  ## Background Metals Weighted (Probmon Data) Section ##
  #-------------------------------------------------------------------------------------------
  output$weightedMap <- renderLeaflet({
    leaflet(metalsSites) %>% addProviderTiles('Thunderforest.Landscape') %>%
      fitBounds(~min(LongitudeDD),~min(LatitudeDD),~max(LongitudeDD),~max(LatitudeDD))%>%
      #fitBounds(metalsSites1@bbox)
      addMouseCoordinates()%>%#style='basic')%>%
      addHomeButton(extent(metalsSites1), "Virginia")
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
                           list(extend='csv',filename=paste('BackgroundWeightedMetals_',input$metalToPlot,Sys.Date(),sep='')),
                           list(extend='excel',filename=paste('BackgroundWeightedMetals_',input$metalToPlot,Sys.Date(),sep='')))))})                         
  
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
  activeDotUN <- function(map,x,y){addCircleMarkers(map,x,y,radius=6,color='black',fillColor = 'yellow',
                                                  fillOpacity = 1,opacity=1,weight = 2,stroke=T,layerId = 'SelectedUN')}
  
  
  output$unweightedMap <- renderLeaflet({
    if(input$targetlocationUN==""){
      leaflet() %>% addProviderTiles('Thunderforest.Landscape') %>%
        addMouseCoordinates()%>%#style='basic')%>%
        addHomeButton(extent(metalsSites1), "Virginia")%>%
        addCircleMarkers(data=metalsSites1,radius=6,
                         color=~'black',stroke=F,fillOpacity=0.5,
                         group='selectedSites_UN',layerId=~StationID_Trend,
                         popup=popupTable(metalsSites1, zcol = c("StationID","Year","StationID_Trend","CALCIUM","MAGNESIUM","ARSENIC","BARIUM",         
                                                                "BERYLLIUM","CADMIUM","CHROMIUM","COPPER","IRON","LEAD","MANGANESE","THALLIUM",
                                                                "NICKEL","SILVER","ZINC","ANTIMONY","ALUMINUM","SELENIUM","HARDNESS","MERCURY")))
    }else{
      target_pos = geocode(input$targetlocationUN)
      
      leaflet()%>%addProviderTiles('Thunderforest.Landscape')%>%addMouseCoordinates()%>%#style='basic')%>%
        setView(lng=target_pos$lon,lat=target_pos$lat,zoom=10)%>%
        addCircleMarkers(data=metalsSites1,radius=6,
                         color=~'black',stroke=F,fillOpacity=0.5,
                         group='selectedSites_UN',layerId=~StationID_Trend,
                         popup=popupTable(metalsSites1, zcol = c("StationID","Year","StationID_Trend","CALCIUM","MAGNESIUM","ARSENIC","BARIUM",         
                                                                "BERYLLIUM","CADMIUM","CHROMIUM","COPPER","IRON","LEAD","MANGANESE","THALLIUM",
                                                                "NICKEL","SILVER","ZINC","ANTIMONY","ALUMINUM","SELENIUM","HARDNESS","MERCURY")))
    }
  })
  
  ## Move map view to adjust with marker click ##
  observeEvent(input$unweightedMap_marker_click,{
    click <- input$unweightedMap_marker_click
    proxy <- leafletProxy("unweightedMap")
    if(click$id=="SelectedUN"){
      proxy%>%removeMarker(layerId='SelectedUN')
    }else{
      proxy %>% setView(lng=click$lng,
                        lat=ifelse(input$unweightedMap_zoom<10,click$lat+(3/input$unweightedMap_zoom),click$lat),
                        input$unweightedMap_zoom)%>%
        activeDotUN(click$lng,click$lat)
    }
  })
  
  ## Plot Facility on map ##
  observeEvent(input$runStats,{
    lat <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][1]))
    lng <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][2]))
    # make a spatial object from lat/long
    point <- data.frame(name='userPoint',lat=lat,lng=lng)
    coordinates(point) <- ~lng+lat
    proj4string(point) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")       
    
    leafletProxy('unweightedMap') %>% clearMarkers() %>% clearControls() %>%
      setView(lng=lng,lat=lat,zoom=9)%>%
      addCircleMarkers(data=metalsSites1,radius=6,
                       color=~'black',stroke=F,fillOpacity=0.5,
                       group='selectedSites_UN',layerId=~StationID_Trend,
                       popup=popupTable(metalsSites1, zcol = c("StationID","Year","StationID_Trend","CALCIUM","MAGNESIUM","ARSENIC","BARIUM",         
                                                               "BERYLLIUM","CADMIUM","CHROMIUM","COPPER","IRON","LEAD","MANGANESE","THALLIUM",
                                                               "NICKEL","SILVER","ZINC","ANTIMONY","ALUMINUM","SELENIUM","HARDNESS","MERCURY")))%>%
      addCircleMarkers(data=point,radius=8,
                       color=~'red',stroke=F,fillOpacity=0.5,
                       group='selectedSites_UN',layerId='Facility',popup='User Facility')
    
  })
  
  ## Add polygons ##
  observe({if(input$supaBshape==T){
    leafletProxy('unweightedMap')%>%
      addPolygons(data=Superbasins,color='blue',fill=0.9,stroke=0.1,group="Superbasins_",
                  popup=paste("Superbasin: ",Superbasins@data$NAME,sep=""))}else(leafletProxy('unweightedMap')%>%clearGroup("Superbasins_"))})
  
  observe({if(input$ecoshape==T){
    leafletProxy('unweightedMap')%>%
      addPolygons(data=Ecoregions,color='grey',fill=0.9,stroke=0.1,group="Ecoregions_",
                  popup=paste("Ecoregion: ",Ecoregions@data$NAME,sep=""))}else(leafletProxy('unweightedMap')%>%clearGroup("Ecoregions_"))})
  
  observe({if(input$hucshape==T){
    leafletProxy('unweightedMap')%>%
      addPolygons(data=huc8,color='orange',fill=0.9,stroke=0.1,group="huc_",
                  popup=paste(sep="<br/>",paste("HUC8: ",huc8@data$CU,sep=""),
                              paste('Name: ',capwords(tolower(huc8@data$NAME)),sep="")))}else(leafletProxy('unweightedMap')%>%clearGroup("huc_"))})
  
  
  ## grey out runStats button until both fields are filled in ##
  observe({
    lat <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][1]))
    lng <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][2]))
    shinyjs::toggleState('runStats', input$facilityUN !="" && input$metalToPlotUN != "No Metals" &&
                           sum(findInterval(lat,bbox(Superbasins)[2,]),findInterval(lng,bbox(Superbasins)[1,]))>1)
  })
  

  basin <- eventReactive(input$runStats,{
    geogsub(input$facilityUN,Superbasins,input$metalToPlotUN)})
  output$basinTable <- DT::renderDataTable({
    if(is.null(basin()))
      return(NULL)
    datatable(basin(),
              extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',
                           buttons=list('copy',
                                        list(extend='csv',filename=paste('BackgroundUnweightedMetals_',input$metalToPlotUN,Sys.Date(),sep='')),
                                        list(extend='excel',filename=paste('BackgroundUnweightedMetals_',input$metalToPlotUN,Sys.Date(),sep='')))))})
  
  eco <- eventReactive(input$runStats,{
    x <- geogsub(input$facilityUN,Ecoregions,input$metalToPlotUN)
    if(nrow(x)>0){return(x%>%dplyr::rename(Ecoregion=Watershed))}else{return(x)}})
  output$ecoTable <- DT::renderDataTable({
    if(is.null(eco()))
      return(NULL)
    datatable(eco(),
              extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',
                           buttons=list('copy',
                                        list(extend='csv',filename=paste('BackgroundUnweightedMetals_',input$metalToPlotUN,Sys.Date(),sep='')),
                                        list(extend='excel',filename=paste('BackgroundUnweightedMetals_',input$metalToPlotUN,Sys.Date(),sep='')))))})
  
  huc <- eventReactive(input$runStats,{
    geogsub(input$facilityUN,huc8,input$metalToPlotUN)})
  output$huc8Table <- DT::renderDataTable({
    if(is.null(huc()))
      return(NULL)
    datatable(huc(),
              extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',
                           buttons=list('copy',
                                        list(extend='csv',filename=paste('BackgroundUnweightedMetals_',input$metalToPlotUN,Sys.Date(),sep='')),
                                        list(extend='excel',filename=paste('BackgroundUnweightedMetals_',input$metalToPlotUN,Sys.Date(),sep='')))))})
  
  
  
  
  # All Stats Summary table #
  allstatsUN <- reactive({
    if(is.null(basin()))
      return(NULL)
    basin1 <- basin()$Watershed[1]
    eco1 <- eco()$Ecoregion[1]
    huc1 <- huc()$Watershed[1]
    basin2 <- filter(allstatsdataUN,Population %in% basin1)#c(basin1,eco1,huc1))
    eco2 <- filter(allstatsdataUN,Population %in% eco1)
    huc2 <- filter(allstatsdataUN,Population %in% huc1)
    return(rbind(basin2,eco2,huc2)%>%arrange(Metal))
   })
  
  # Facility location for output report #
  facilityloc <- reactive({
    if(is.null(input$facilityUN))
      return(NULL)
    lat <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][1]))
    lng <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][2]))
    return(paste(lat,lng,sep=' , '))
  })
  
  
  ## grey out reviewstatsUN button until both fields are filled in ##
  observe({
    lat <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][1]))
    lng <- as.numeric(gsub(" ","",strsplit(input$facilityUN,",")[[1]][2]))
    shinyjs::toggleState('reviewstatsUN', input$facilityUN !="" && input$metalToPlotUN != "No Metals" &&
                           sum(findInterval(lat,bbox(Superbasins)[2,]),findInterval(lng,bbox(Superbasins)[1,]))>1)
  })
  
  # Modal preview of data to knit to report #
  observeEvent(input$reviewstatsUN,{
    showModal(modalDialog(
      title="Statistics for all metals (Preview)",
      h5("For faster app rendering, this is just a preview  of your selected data.",
         span(strong("Click the 'Send to Report' button to review the entire dataset."))),
      hr(),
      tableOutput('allstatstableUN'),
      br(),
      downloadButton('knitUN','Send to Report'),
      easyClose = TRUE
    ))
  })
  
  output$allstatstableUN <- renderTable({
    df <- allstatsUN()[1:10,1:7]
    df$n <- format(df$n,digits=1)
    names(df) <- c('Metal','population','n','5%','10%','25%','50%')
    return(df)
  })
  
  
  ##---------------------------------------RMARKDOWN SECTION----------------------------------------------
  
  output$knitUN <- downloadHandler(
    'UnweightedResults.html',
    content= function(file){
      tempReport <- file.path(tempdir(), "UnweightedreportHTML.Rmd")
      file.copy("UnweightedreportHTML.Rmd",tempReport,overwrite=T)
      params <- list(table_allstatsUN=allstatsUN(),facilitylocation=facilityloc())
      
      rmarkdown::render(tempReport,output_file= file,
                        params=params, envir=new.env(parent=globalenv()))})
  ##------------------------------------------------------------------------------------------------------
  
  
  
  
})

